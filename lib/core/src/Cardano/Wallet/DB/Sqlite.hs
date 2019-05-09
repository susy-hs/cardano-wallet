{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Sqlite
    ( newDBLayer
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.DB.SqliteTypes
    ( AddressScheme (..), TxId )
import Control.Exception
    ( try )
import Control.Monad
    ( void )
import qualified Data.ByteString.Char8 as B8
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Text
    ( Text )
import qualified Data.Text as T
import Data.Time.Clock
    ( UTCTime )
import Data.Word
    ( Word32 )
import Database.Persist.TH
import GHC.Generics
    ( Generic (..) )
import System.IO
    ( stderr )
import System.Log.FastLogger
    ( fromLogStr )

import Conduit
    ( MonadUnliftIO, ResourceT, runResourceT )
import Control.Monad.Logger
    ( LoggingT, runNoLoggingT, runStderrLoggingT )
import Control.Monad.Reader
    ( ReaderT )
import Database.Persist.Sqlite
    -- ( SqlBackend, runMigration, runSqlConn, withSqliteConn )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Database.Persist.Sql
    ( LogFunc )
import Database.Persist.Sqlite
    ( createSqlPool, wrapConnection )
import qualified Database.Sqlite as Sqlite

import Cardano.Wallet.Primitive.Types
    ( WalletMetadata (..) )

import qualified Cardano.Wallet.Primitive.Types as W

-- fixme: need tables for wallet AddressPool

share
    [ mkPersist sqlSettings { mpsPrefixFields = False }
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|

-- Wallet IDs, address discovery state, and metadata.
Wallet
    walTableId                 W.WalletId     sql=wallet_id
    walTableName               Text           sql=name
    walTablePassLastUpdatedAt  UTCTime        sql=passphrase_last_updated_at
    walTableStatus             W.WalletState  sql=status
    walTableDelegation         Text Maybe     sql=delegation
    walTableAddressScheme      AddressScheme  sql=address_discovery

    Primary walTableId
    deriving Show Generic

-- The private key for each wallet. This is in a separate table simply so that
-- "SELECT * FROM wallet" won't print keys.
PrivateKey                               sql=private_key
    privateKeyTableWalletId  W.WalletId  sql=wallet_id
    privateKeyTableKey       Text        sql=private_key

    Primary privateKeyTableWalletId
    Foreign Wallet fk_wallet_private_key privateKeyTableWalletId

    deriving Show Generic

-- Maps a transaction ID to its metadata (which is calculated when applying
-- blocks).
--
-- TxMeta is specific to a wallet because multiple wallets may have the same
-- transaction with different metdata values. The associated inputs and outputs
-- of the transaction are in the TxIn and TxOut tables.
TxMeta
    txMetaTableTxId       TxId         sql=tx_id
    txMetaTableWalletId   W.WalletId   sql=wallet_id
    txMetaTableStatus     W.TxStatus   sql=status
    txMetaTableDirection  W.Direction  sql=direction
    txMetaTableSlotId     W.SlotId     sql=slot_id
    txMetaTableAmount     W.Coin       sql=amount

    Primary txMetaTableTxId txMetaTableWalletId
    Foreign Wallet fk_wallet_tx_meta txMetaTableWalletId
    deriving Show Generic

-- A transaction input associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txInputTableTxId is referred to by TxMeta and PendingTx
TxIn
    txInputTableTxId         TxId        sql=tx_id
    txInputTableWalletId     W.WalletId  sql=wallet_id
    txInputTableSourceTxId   TxId        sql=source_id
    txInputTableSourceIndex  Word32      sql=source_index

    Primary txInputTableTxId txInputTableSourceTxId txInputTableSourceIndex
    Foreign TxMeta fk_tx_meta_tx_in txInputTableTxId txInputTableWalletId
    deriving Show Generic

-- A transaction output associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txOutputTableTxId is referred to by TxMeta and PendingTx
TxOut
    txOutputTableTxId     TxId        sql=tx_id
    txOutputTableWalletId W.WalletId  sql=wallet_id
    txOutputTableIndex    Word32      sql=index
    txOutputTableAddress  Text        sql=address
    txOutputTableAmount   W.Coin      sql=amount

    Primary txOutputTableTxId txOutputTableIndex
    Foreign TxMeta fk_tx_meta_tx_out txOutputTableTxId txOutputTableWalletId
    deriving Show Generic

-- A checkpoint for a given wallet is referred to by (wallet_id, slot_id).
-- Checkpoint data such as UTxO will refer to this table.
Checkpoint
    checkpointTableWalletId    W.WalletId  sql=wallet_id
    checkpointTableWalletSlot  W.SlotId    sql=slot_id

    Primary checkpointTableWalletId checkpointTableWalletSlot
    Foreign Wallet fk_wallet_checkpoint checkpointTableWalletId

    deriving Show Generic

-- The UTxO for a given wallet checkpoint is a one-to-one mapping from TxIn ->
-- TxOut. This table does not need to refer to the TxIn or TxOut tables. All
-- necessary information for the UTxO is in this table.
UTxO                                  sql=utxo

    -- The wallet checkpoint (wallet_id, slot_id)
    utxoTableWalletId     W.WalletId  sql=wallet_id
    utxoTableWalletSlot   W.SlotId    sql=slot_id

    -- TxIn
    utxoTableInputId      TxId        sql=input_tx_id
    utxoTableInputIndex   Word32      sql=input_index

    -- TxOut
    utxoTableOutputId     TxId        sql=output_tx_id
    utxoTableOutputIndex  Word32      sql=output_index

    Primary
        utxoTableWalletId
        utxoTableWalletSlot
        utxoTableInputId
        utxoTableInputIndex
        utxoTableOutputId
        utxoTableOutputIndex

    Foreign Checkpoint fk_checkpoint_utxo utxoTableWalletId utxoTableWalletSlot
    deriving Show Generic
|]

enableForeignKeys :: Sqlite.Connection -> IO ()
enableForeignKeys conn = Sqlite.prepare conn "PRAGMA foreign_keys = ON;" >>= void . Sqlite.step

createSqliteBackend :: Maybe FilePath -> LogFunc -> IO SqlBackend
createSqliteBackend fp logFunc = do
  conn <- Sqlite.open (sqliteConnStr fp)
  enableForeignKeys conn
  wrapConnection conn logFunc

sqliteConnStr :: Maybe FilePath -> Text
sqliteConnStr = maybe ":memory:" T.pack

newDBLayer :: forall s t. Maybe FilePath -> IO (DBLayer IO s t)
newDBLayer fp = do
    conn <- createSqliteBackend fp logStderr
    unsafeRunQuery conn $ runMigration migrateAll
    return $ DBLayer
        { createWallet = \(PrimaryKey wid) _cp meta -> ExceptT $ unsafeRunQuery conn $ do
                insert (mkWalletEntity wid meta)
                pure $ Right ()
        , removeWallet = \(PrimaryKey wid) -> ExceptT $ unsafeRunQuery conn $ do
                n <- deleteWhereCount [WalId ==. wid]
                pure $ if n == 0
                       then Left (ErrNoSuchWallet wid)
                       else Right ()
        , listWallets = unsafeRunQuery conn $
            map (PrimaryKey . walId . entityVal) <$> selectList [] []
            -- fixme: would like to use selectKeysList [] []
        }

logStderr :: LogFunc
logStderr _ _ _ str = B8.hPutStrLn stderr (fromLogStr str)

-- | Run a query without error handling. There will be exceptions thrown.
unsafeRunQuery
    :: SqlBackend
    -> SqlPersistM a
    -> IO a
unsafeRunQuery conn = runResourceT . runNoLoggingT . flip runSqlConn conn

runQuery
    :: SqlBackend
    -> SqlPersistM a
    -> ExceptT Sqlite.SqliteException IO a
runQuery conn = ExceptT . try . runResourceT . runNoLoggingT . flip runSqlConn conn

mkWalletEntity :: W.WalletId -> W.WalletMetadata -> Wallet
mkWalletEntity wid md = Wallet
    { walId = wid
    , walName = W.getWalletName (md ^. #name)
    , walPassphraseLastUpdatedAt = W.lastUpdatedAt (md ^. #passphraseInfo)
    , walStatus = md ^. #status
    , walDelegation = Nothing
    , walAddressScheme = Sequential -- fixme: depends on wallet
    }
