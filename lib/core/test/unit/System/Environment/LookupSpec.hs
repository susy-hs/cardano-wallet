{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Environment.LookupSpec where

import Prelude

import Data.Maybe
    ( isNothing )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import GHC.Generics
    ( Generic )
import System.Environment
    ( setEnv, unsetEnv )
import System.Environment.Lookup
    ( ErrMissingOrInvalidEnvVar (..), unsafeLookupEnv )
import Test.Hspec
    ( Spec, describe, it, shouldThrow )
import Test.QuickCheck
    ( Arbitrary (..) )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )

import qualified Data.Text as T

spec :: Spec
spec = do
    describe "ErrMissingOrInvalidEnvVar (Show / displayException)" $ do
        let errNoAdditionalContext = ErrMissingOrInvalidEnvVar
                { name = "PATATE"
                , command = "my-command"
                , additionalContext = Nothing
                }
        let errWithAdditionalContext = ErrMissingOrInvalidEnvVar
                { name = "PATATE"
                , command = "my-command"
                , additionalContext = Just
                    ("💩"
                    , TextDecodingError
                        { getTextDecodingError = "not a valid value" }
                    )
                }
        it (show errNoAdditionalContext) True
        it (show errWithAdditionalContext) True

    describe "unsafeLookupEnv" $ do
        it "throws with no context when variable isn't present" $ do
            unsetEnv "PATATE" -- Just in case
            let io =
                    unsafeLookupEnv @Network "PATATE" `seq` (return ())
            let selector (ErrMissingOrInvalidEnvVar n _ c) =
                    n == "PATATE" && isNothing c
            io `shouldThrow` selector

        it "throws with extra context when variable is present but invalid" $ do
            setEnv "PATATE" "not-a-network"
            let ctx =
                    ( "not-a-network"
                    , TextDecodingError "not-a-network is neither \"mainnet\",\
                        \ \"testnet\" nor \"staging\"."
                    )
            let selector (ErrMissingOrInvalidEnvVar n _ c) =
                    n == "PATATE" && c == Just ctx
            let io =
                    unsafeLookupEnv @Network "PATATE" `seq` (return ())
            io `shouldThrow` selector

{-------------------------------------------------------------------------------
                              Types
-------------------------------------------------------------------------------}

data Network = Mainnet | Testnet | Staging
    deriving Generic

instance Arbitrary Network where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance FromText Network where
    fromText = \case
        "mainnet" -> Right Mainnet
        "testnet" -> Right Testnet
        "staging" -> Right Staging
        s -> Left $ TextDecodingError $ T.unpack s
            <> " is neither \"mainnet\", \"testnet\" nor \"staging\"."

instance ToText Network where
    toText = \case
        Mainnet -> "mainnet"
        Testnet -> "testnet"
        Staging -> "staging"
