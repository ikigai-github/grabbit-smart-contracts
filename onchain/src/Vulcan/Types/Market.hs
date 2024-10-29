{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vulcan.Types.Market (
  PMarketTerms (..),
  PMarketRedeemer (..),
) where

import Plutarch.Api.V1 (PAddress)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))

import Plutarch.Positive (PPositive)
import Vulcan.Common.Types.Auction (MarketTerms)

-- For details see specification documents and Vulcan.Common.Types module(s).

newtype PMarketTerms (s :: S)
  = PMarketTerms
      ( Term
          s
          ( PDataRecord
              '[ "fixedFee" ':= PInteger
               , "percentageFee" ':= PInteger
               , "feeAddress" ':= PAddress
               , "minAda" ':= PPositive
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

deriving anyclass instance
  PTryFrom PData PMarketTerms

deriving anyclass instance
  PTryFrom PData (PAsData PMarketTerms)

instance DerivePlutusType PMarketTerms where type DPTStrat _ = PlutusTypeData

data PMarketRedeemer (s :: S)
  = PUpdateTerms (Term s (PDataRecord '["terms" ':= PMarketTerms]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PMarketRedeemer where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PMarketRedeemer)

instance PUnsafeLiftDecl PMarketTerms where
  type PLifted PMarketTerms = MarketTerms
deriving via
  (DerivePConstantViaData MarketTerms PMarketTerms)
  instance
    PConstantDecl MarketTerms
