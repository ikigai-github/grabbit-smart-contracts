{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vulcan.Types.State (
  PPrivStateMintingRedeemer (..),
  PPubStateMintingRedeemer (..),
) where

import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Vulcan.Common.Types.Auction (PrivStateMintingRedeemer, PubStateMintingRedeemer)

-- For details see specification documents and Vulcan.Common.Types module(s).

--------------------------------------
-- State MP Redeemers

data PPrivStateMintingRedeemer (s :: S)
  = PAnnounceAuction (Term s (PDataRecord '[]))
  | PRegistration (Term s (PDataRecord '[]))
  | PAuctionLifeCycle (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PPrivStateMintingRedeemer where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PPrivStateMintingRedeemer)

data PPubStateMintingRedeemer (s :: S)
  = PPubAnnounceAuction (Term s (PDataRecord '[]))
  | PPubEnrollBidder (Term s (PDataRecord '[]))
  | PPubAuctionLifeCycle (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PPubStateMintingRedeemer where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PPubStateMintingRedeemer)

instance PUnsafeLiftDecl PPrivStateMintingRedeemer where
  type PLifted PPrivStateMintingRedeemer = PrivStateMintingRedeemer
deriving via
  (DerivePConstantViaData PrivStateMintingRedeemer PPrivStateMintingRedeemer)
  instance
    PConstantDecl PrivStateMintingRedeemer

instance PUnsafeLiftDecl PPubStateMintingRedeemer where
  type PLifted PPubStateMintingRedeemer = PubStateMintingRedeemer
deriving via
  (DerivePConstantViaData PubStateMintingRedeemer PPubStateMintingRedeemer)
  instance
    PConstantDecl PubStateMintingRedeemer
