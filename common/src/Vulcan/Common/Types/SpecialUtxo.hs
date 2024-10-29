{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Vulcan.Common.Types.SpecialUtxo (
  SpecialUTxOTag (
    AuctionEscrowUTxO,
    BidEscrowUTxO,
    RegistrationEscrowUTxO
  ),
  NamedToken (tokenName),
  nameToken,
  UTxO (..),
  StateToken,
  findCS,
  ContainsToken,
) where

import Data.Kind (Type)

import Ledger.Exports.V1 qualified as V1
import Ledger.Exports.V2 (
  CurrencySymbol,
  StakingCredential,
  TokenName,
  Value,
 )
import PlutusTx (unstableMakeIsData)

import Vulcan.Common.Types.Auction (
  AuctionEscrow,
  BidEscrow,
  RegistrationEscrow,
 )
import Vulcan.Common.Utils qualified as Utils

type UTxO :: Type -> Type
data UTxO t = MkUTxO
  { {- in pure representation we don't wrap
        it in MarketValidatorDatum sum type -}
    datum :: t
  , extraValue :: Value
  , staking :: Maybe StakingCredential
  }
  deriving stock (Eq, Show)

unstableMakeIsData ''UTxO

class NamedToken t => ContainsToken t
instance ContainsToken AuctionEscrow
instance ContainsToken BidEscrow

findCS :: forall t. ContainsToken t => Value -> Maybe CurrencySymbol
findCS =
  Utils.findWith (\(cs, tn, _) -> cs <$ Utils.satisfies (== tokenName @t) tn)
    . V1.flattenValue

class NamedToken t where
  tokenName :: TokenName
instance NamedToken AuctionEscrow where
  tokenName = V1.tokenName "AuctionEscrow"
instance NamedToken BidEscrow where
  tokenName = V1.tokenName "BidEscrow"

nameToken :: forall t. NamedToken t => CurrencySymbol -> V1.AssetClass
nameToken = V1.AssetClass . (,tokenName @t)

class NamedToken t => StateToken t
instance StateToken BidEscrow
instance StateToken AuctionEscrow

data SpecialUTxOTag (t :: Type) where
  AuctionEscrowUTxO :: SpecialUTxOTag AuctionEscrow
  BidEscrowUTxO :: SpecialUTxOTag BidEscrow
  RegistrationEscrowUTxO :: SpecialUTxOTag RegistrationEscrow
