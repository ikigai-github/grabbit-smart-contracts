{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Vulcan.Common.Types.Instances (
  CompiledScript (..),
  CompiledMarketInstance,
  CompiledInstance,
  AuctionScripts (..),
  AuctionInstance,
  AuctionInstance' (..),
  MarketInstance,
  MarketInstance' (..),
  Config (..),
  FinSetScript (..),
  AuctionParams (..),
  TracingMode (..),
  Verbosity (..),
) where

import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (bimap))
import Data.Kind (Type)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Ledger.Exports.V2 (
  CurrencySymbol,
  MintingPolicy,
  TxOutRef,
  Validator,
  ValidatorHash,
 )
import Ledger.Orphans ()
import Ply (
  ScriptRole (MintingPolicyRole, ValidatorRole),
  TypedScript,
 )
import Vulcan.Common.Types.FinSet (SeparatorConfig)

{- Almost the copy from Plutarch.Internal.Config with wider variety of options
  We need that to avoid $common$ depends on $plutarch$ and to control traces verbosity.
-}
data TracingMode = NoTracing | DoTracing Verbosity
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data Verbosity
  = Verbose
  | Conscise
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

newtype Config = Config
  { tracingMode :: TracingMode
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | Parameters that are sufficient to initiate an Auction from
type AuctionParams :: Type
data AuctionParams = MkAuctionParams
  { oref :: TxOutRef
  , isPrivate :: Bool
  , config :: Config
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

type MarketInstance = MarketInstance' Validator MintingPolicy
data MarketInstance' a b = MkMarketInstance
  { marketValidator :: a
  , marketMP :: b
  }
  deriving stock (Eq, Generic, Functor, Show)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

type AuctionInstance = AuctionInstance' Validator MintingPolicy
data AuctionInstance' a b = MkAuctionInstance
  { auctionValidator :: a
  , nodeValidator :: a
  , nodeMP :: b
  , stateMP :: b
  }
  deriving stock (Eq, Generic, Functor, Show)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data AuctionScripts = MkAuctionScripts
  { auctionScript :: TypedScript 'ValidatorRole '[]
  , nodeScript :: TypedScript 'ValidatorRole '[]
  , nodeMPScript :: FinSetScript
  , stateMPScript :: TypedScript 'MintingPolicyRole '[CurrencySymbol, TxOutRef]
  }

data FinSetScript
  = StandardFinSet (TypedScript 'MintingPolicyRole '[TxOutRef])
  | SeparatorFinSet (TypedScript 'MintingPolicyRole '[TxOutRef, SeparatorConfig])

type CompiledScript :: Type -> Type
data CompiledScript a = MkCompiledScript
  { scriptCborHex :: T.Text
  , version :: T.Text
  , hash :: a
  }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

type CompiledInstance :: Type
type CompiledInstance =
  AuctionInstance'
    (CompiledScript ValidatorHash)
    (CompiledScript CurrencySymbol)

type CompiledMarketInstance :: Type
type CompiledMarketInstance =
  MarketInstance'
    (CompiledScript ValidatorHash)
    (CompiledScript CurrencySymbol)

instance Bifunctor MarketInstance' where
  bimap f g (fmap g -> MkMarketInstance {..}) =
    MkMarketInstance
      { marketValidator = f marketValidator
      , marketMP
      }

instance Bifunctor AuctionInstance' where
  bimap f g (fmap g -> MkAuctionInstance {..}) =
    MkAuctionInstance
      { auctionValidator = f auctionValidator
      , nodeValidator = f nodeValidator
      , nodeMP
      , stateMP
      }
