{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vulcan.Types.Auction (
  PPositive,
  PAuctionTerms (..),
  PRegistrationEscrow (..),
  PAuctionEscrow (..),
  PBidEscrow (..),
  PBidInfo (..),
  PBidStatus (..),
  PPrivAuctionRedeemer (..),
  PPubAuctionRedeemer (..),
  PCancelReason (..),
  PRefundReason (..),
) where

import Plutarch.Api.V1 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted, Unsorted),
  PCurrencySymbol,
  PMap,
  PPOSIXTime,
  PValue,
 )
import Plutarch.Api.V2 (
  PAddress,
  PMaybeData,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Ply.Plutarch (PlyArgOf)

import Plutarch.Positive (PPositive)
import Vulcan.Common.Types.Auction (
  AuctionEscrow,
  AuctionTerms,
  BidEscrow,
  PrivAuctionRedeemer,
  PubAuctionRedeemer,
  RegistrationEscrow,
 )

-- For details see specification documents and Vulcan.Common.Types module(s).

---------------------------------------
-- Auction UTxOs
---------------------------------------

-- Auction Escrow UTxO Datum:

newtype PAuctionEscrow (s :: S)
  = PAuctionEscrow
      ( Term
          s
          ( PDataRecord
              '[ "terms" ':= PAuctionTerms
               , "nodeCS" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PAuctionEscrow where type DPTStrat _ = PlutusTypeData

newtype PAuctionTerms (s :: S)
  = PAuctionTerms
      ( Term
          s
          ( PDataRecord
              '[ "lot" ':= PValue 'Sorted 'Positive
               , "auctionInfo" ':= PAuctionInfo
               , "bidInfo" ':= PBidInfo
               , "time" ':= PAuctionTime
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PAuctionTerms where type DPTStrat _ = PlutusTypeData

type instance PlyArgOf PAuctionTerms = AuctionTerms

deriving anyclass instance
  PTryFrom PData PAuctionTerms

deriving anyclass instance
  PTryFrom PData (PAsData PBool)

newtype PAuctionTime (s :: S)
  = PAuctionTime
      ( Term
          s
          ( PDataRecord
              '[ "start" ':= PPOSIXTime
               , "close" ':= PPOSIXTime
               , "extension" ':= PMaybeData PTimeExtension
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PAuctionTime where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PAuctionTime

newtype PTimeExtension (s :: S)
  = PTimeExtension
      ( Term
          s
          ( PDataRecord
              '[ "window" ':= PPositive
               , "length" ':= PPositive
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PTimeExtension where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PTimeExtension

newtype PAuctionInfo (s :: S)
  = PAuctionInfo
      ( Term
          s
          ( PDataRecord
              '[ "seller" ':= PAddress
               , "beneficiaries" ':= PMap 'Unsorted PAddress PPositive
               , "sellerToCover" ':= PBool
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PAuctionInfo where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PAuctionInfo

newtype PBidInfo (s :: S)
  = PBidInfo
      ( Term
          s
          ( PDataRecord
              '[ "buyNowPrice" ':= PPositive
               , "startingPrice" ':= PPositive
               , "raiseMinimum" ':= PPositive
               , "raisePercentage" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PBidInfo where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PBidInfo

deriving anyclass instance
  PTryFrom PData (PAsData PAuctionEscrow)

---------------------------------------
-- Registration Escrow UTxO Datum: Private Auctions.

data PRegistrationEscrow (s :: S)
  = PRegistrationEscrow
      ( Term
          s
          ( PDataRecord
              '[ "registrant" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PRegistrationEscrow where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PRegistrationEscrow)

--------------------------------------
-- Bid Escrow UTxO Datum

newtype PBidEscrow (s :: S)
  = PBidEscrow
      ( Term
          s
          ( PDataRecord
              '[ "status" ':= PBidStatus
               , "bidder" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PBidEscrow where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PBidEscrow)

data PBidStatus (s :: S)
  = PNoBid (Term s (PDataRecord '[]))
  | PBid (Term s (PDataRecord '["bid" ':= PPositive, "time" ':= PPOSIXTime]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PBidStatus where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PBidStatus)

deriving anyclass instance
  PTryFrom PData PBidStatus

---------------------------------------
-- Auction Redeemers:
---------------------------------------

data PPrivAuctionRedeemer (s :: S)
  = PCancelAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol, "reason" ':= PCancelReason]))
  | PBidAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  | PExtendAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  | PCloseExtensionsAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  | PBuyNowAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  | PRefundAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol, "reason" ':= PRefundReason]))
  | PResolveAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  | PUnregisterAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  | PIssuePaddleAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PPrivAuctionRedeemer where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PPrivAuctionRedeemer)

data PPubAuctionRedeemer (s :: S)
  = PPubCancelAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol, "reason" ':= PCancelReason]))
  | PPubBidAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  | PPubExtendAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  | PPubCloseExtensionsAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  | PPubBuyNowAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  | PPubBuyNowNoEnrollAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol, "receiver" ':= PAddress]))
  | PPubRefundAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol, "reason" ':= PRefundReason]))
  | PPubResolveAct (Term s (PDataRecord '["cs" ':= PCurrencySymbol]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PPubAuctionRedeemer where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PPubAuctionRedeemer)

data PCancelReason (s :: S)
  = PFailed (Term s (PDataRecord '[]))
  | PBought (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PCancelReason where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PCancelReason

data PRefundReason (s :: S)
  = PLostBid (Term s (PDataRecord '[]))
  | PBoughtLot (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PRefundReason where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PRefundReason

-----------------------------------------
-- Liftable Instances:

instance PUnsafeLiftDecl PAuctionTerms where
  type PLifted PAuctionTerms = AuctionTerms
deriving via
  (DerivePConstantViaData AuctionTerms PAuctionTerms)
  instance
    PConstantDecl AuctionTerms

instance PUnsafeLiftDecl PBidEscrow where
  type PLifted PBidEscrow = BidEscrow
deriving via
  (DerivePConstantViaData BidEscrow PBidEscrow)
  instance
    PConstantDecl BidEscrow

instance PUnsafeLiftDecl PAuctionEscrow where
  type PLifted PAuctionEscrow = AuctionEscrow
deriving via
  (DerivePConstantViaData AuctionEscrow PAuctionEscrow)
  instance
    PConstantDecl AuctionEscrow

instance PUnsafeLiftDecl PRegistrationEscrow where
  type PLifted PRegistrationEscrow = RegistrationEscrow
deriving via
  (DerivePConstantViaData RegistrationEscrow PRegistrationEscrow)
  instance
    PConstantDecl RegistrationEscrow

instance PUnsafeLiftDecl PPrivAuctionRedeemer where
  type PLifted PPrivAuctionRedeemer = PrivAuctionRedeemer
deriving via
  (DerivePConstantViaData PrivAuctionRedeemer PPrivAuctionRedeemer)
  instance
    PConstantDecl PrivAuctionRedeemer

instance PUnsafeLiftDecl PPubAuctionRedeemer where
  type PLifted PPubAuctionRedeemer = PubAuctionRedeemer
deriving via
  (DerivePConstantViaData PubAuctionRedeemer PPubAuctionRedeemer)
  instance
    PConstantDecl PubAuctionRedeemer
