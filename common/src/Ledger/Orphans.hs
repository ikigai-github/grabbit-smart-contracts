{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Orphans () where

#ifdef NEW_LEDGER_NAMESPACE

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as JSON
import Data.ByteString qualified as BSS
import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  Data,
  POSIXTime,
  PubKeyHash,
  TokenName,
  TxId,
  TxOutRef,
  ValidatorHash,
  Value,
  Address,
  Credential,
  StakingCredential
 )
import PlutusTx.AssocMap (Map)
import PlutusTx.Builtins qualified as PlutusTx

{-
This module only exists because plutus-ledger-api corresponding
to onchain-plutarch version doesn't export ToJSON, FromJSON instances for those types,
unlike the plutus-ledger-api corresponding to offchain.

Did they move those instances to another module?
-}

-- copied from Data.Aeson.Extra from under plutus-ledger-api:

encodeByteString :: BSS.ByteString -> Text.Text
encodeByteString = TE.decodeUtf8 . Base16.encode

tryDecode :: Text.Text -> Either String BSS.ByteString
tryDecode = Base16.decode . TE.encodeUtf8

decodeByteString :: JSON.Value -> JSON.Parser BSS.ByteString
decodeByteString = JSON.withText "ByteString" (either fail pure . tryDecode)

-- copied from Plutus.V1.Ledger.Orphans from under plutus-ledger-api:

instance ToJSON BSS.ByteString where
  toJSON = JSON.String . encodeByteString

instance FromJSON BSS.ByteString where
  parseJSON = decodeByteString

instance ToJSON PlutusTx.BuiltinByteString where
  toJSON = JSON.String . encodeByteString . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinByteString where
  parseJSON = fmap PlutusTx.toBuiltin . decodeByteString

-- Plutus

deriving anyclass instance FromJSON Data
deriving anyclass instance (FromJSON a, FromJSON b) => FromJSON (Map a b)
deriving anyclass instance FromJSON Value
deriving anyclass instance FromJSON TokenName
deriving anyclass instance FromJSON TxId
deriving anyclass instance FromJSON POSIXTime
deriving anyclass instance FromJSON CurrencySymbol
deriving anyclass instance FromJSON ValidatorHash
deriving anyclass instance FromJSON PubKeyHash
deriving anyclass instance FromJSON TxOutRef
deriving anyclass instance FromJSON Credential
deriving anyclass instance FromJSON StakingCredential
deriving anyclass instance FromJSON Address

deriving anyclass instance ToJSON Data
deriving anyclass instance (ToJSON a, ToJSON b) => ToJSON (Map a b)
deriving anyclass instance ToJSON Value
deriving anyclass instance ToJSON TokenName
deriving anyclass instance ToJSON TxId
deriving anyclass instance ToJSON POSIXTime
deriving anyclass instance ToJSON CurrencySymbol
deriving anyclass instance ToJSON ValidatorHash
deriving anyclass instance ToJSON PubKeyHash
deriving anyclass instance ToJSON TxOutRef
deriving anyclass instance ToJSON Credential
deriving anyclass instance ToJSON StakingCredential
deriving anyclass instance ToJSON Address

#else


#endif
