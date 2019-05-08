{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- The format is for the Shelley era as implemented by the Jörmungandr node.

module Cardano.Wallet.Binary.Jormungandr
    ( decodeBlockHeader
    , genesisBlock
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ()
import Data.Binary.Get
    ( Get, getByteString, getWord16be, getWord32be )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Debug.Trace
    ( traceShow )

import qualified Data.ByteString as BS

decodeBlockHeader :: Get ()
decodeBlockHeader = do
    headerSize <- getWord16be
    version <- getWord16be
    sizeOfContent <- getWord32be

    slotEpoch <- getWord32be
    slotId <- getWord32be

    chainLength <- getWord32be

    contentHash <- getByteString 32 -- or 256 bits

    parentHeaderHash <- getParentHeaderHash

    -- TODO: Handle special case for BFT
    -- TODO: Handle special case for Praos/Genesis

    traceLog "headerSize:" headerSize
    traceLog "version:" version
    traceLog "sizeOfContent:" sizeOfContent
    traceLog "slotEpoch:" slotEpoch
    traceLog "slotId:" slotId
    traceLog "chainLength:" chainLength
    traceLog "contentHash:" contentHash
    traceLog "parentHeaderHash:" parentHeaderHash

    return (headerSize, version)
  where
    traceLog a b = traceShow (a ++ " " ++ show b ) return ()

getParentHeaderHash :: Get (Maybe ByteString)
getParentHeaderHash = getByteString 32 >>= \case
    a | a == BS.pack (replicate 32 0)
        -> return Nothing
      | otherwise
        -> return $ Just a



-- For development

genesisBlock :: ByteString
genesisBlock = either error id $ convertFromBase @ByteString Base16
    "005200000000009f000000000000000000000000ffadebfecd59d9eaa12e903a\
    \d58100f7c1e35899739c3d05d022835c069d2b4f000000000000000000000000\
    \00000000000000000000000000000000000000000047000048000000005cc1c2\
    \4900810200c200010108000000000000087001410f01840000000a01e030a694\
    \b80dbba2d1b8a4b55652b03d96315c8414b054fa737445ac2d2a865c76002604\
    \0001000000ff0005000006000000000000000000000000000000000000000000\
    \0000000000002c020001833324c37869c122689a35917df53a4f2294a3a52f68\
    \5e05f5f8e53b87e7ea452f000000000000000e"

