{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Vulcan.Common.Types.FinSet (
  NodeAction (Init, Deinit, Insert, Remove, RemoveAndDeinit),
  SepNodeAction (..),
  NodeKey (..),
  SetNode (..),
  SeparatorConfig (..),
  setNodePrefix,
  corrNodePrefix,
  setNodePrefix',
  corrNodePrefix',
) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import GHC.Generics (Generic)

import Ledger.Exports.V2 (BuiltinByteString, PubKeyHash)
import Ledger.Exports.V2 qualified as Ledger

import Ledger.Orphans ()
import PlutusTx (unstableMakeIsData)
import PlutusTx.Prelude qualified as PlutusTx
import Ply (PlyArg)
import Ply.Core.Class (PlyArg (UPLCRep, toBuiltinArg, toBuiltinArgData))

data NodeKey = Key BuiltinByteString | Empty
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data SetNode = MkSetNode
  { key :: NodeKey
  , next :: NodeKey
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data SeparatorConfig = MkSeperatorConfig
  { signer :: Ledger.PubKeyHash
  , cutOff :: Ledger.POSIXTime
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance PlyArg SeparatorConfig where
  type UPLCRep SeparatorConfig = Ledger.Data
  toBuiltinArg
    ( MkSeperatorConfig
        address
        cutOff
      ) =
      Ledger.Constr
        0
        [ toBuiltinArgData address
        , toBuiltinArgData cutOff
        ]
  toBuiltinArgData = toBuiltinArg
data NodeAction
  = Init
  | Deinit
  | -- | first arg is the key to insert, second arg is the covering node
    Insert PubKeyHash SetNode
  | -- | first arg is the key to remove, second arg is the covering node
    Remove PubKeyHash SetNode
  | RemoveAndDeinit PubKeyHash
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data SepNodeAction
  = SepInit
  | SepDeinit
  | SepInsert PubKeyHash SetNode
  | SepRemove PubKeyHash SetNode
  | SepRemoveAndDeinit PubKeyHash
  | InsertSeps [BuiltinByteString] SetNode
  | -- | first arg is the key to insert, second arg is the covering node
    RemoveSeps [BuiltinByteString] SetNode
  -- first arg is the key to remove, second arg is the covering node
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

setNodePrefix :: ByteString
setNodePrefix = "FSN"

corrNodePrefix :: ByteString
corrNodePrefix = "FCN"

corrNodePrefix' :: BuiltinByteString
corrNodePrefix' = PlutusTx.toBuiltin corrNodePrefix

setNodePrefix' :: BuiltinByteString
setNodePrefix' = PlutusTx.toBuiltin setNodePrefix

unstableMakeIsData ''SeparatorConfig
unstableMakeIsData ''NodeKey
unstableMakeIsData ''SetNode
unstableMakeIsData ''SepNodeAction
unstableMakeIsData ''NodeAction
