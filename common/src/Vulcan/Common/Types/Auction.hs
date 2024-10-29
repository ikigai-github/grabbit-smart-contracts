{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Vulcan.Common.Types.Auction (
  Positive,
  AuctionTime (..),
  AuctionTerms (..),
  MarketTerms (..),
  RegistrationEscrow (..),
  AuctionEscrow (..),
  AuctionInfo (..),
  BidInfo (..),
  BidEscrow (..),
  TimeExtension (..),
  BidStatus (..),
  RefundReason (..),
  CancelReason (..),
  PrivAuctionRedeemer (..),
  PubAuctionRedeemer (..),
  PrivStateMintingRedeemer (..),
  PubStateMintingRedeemer (..),
) where

import Ledger.Exports.V2 (
  Address,
  CurrencySymbol,
  Map,
  POSIXTime,
  Value,
 )

import PlutusTx (unstableMakeIsData)

import Data.Aeson qualified as Aeson
import GHC.Generics (Generic)
import Ledger.Orphans ()

type Positive = Integer

data AuctionTerms = MkAuctionTerms
  { lot :: Value
  , auctionInfo :: AuctionInfo
  , bidInfo :: BidInfo
  , time :: AuctionTime
  -- ^ whether Lot was bought using BuyNow feature or not
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data AuctionTime = MkAuctionTime
  { start :: POSIXTime
  -- ^ The time, FROM which auction is open
  , close :: POSIXTime
  -- ^ The time, FROM which auction is closed
  , extension :: Maybe TimeExtension
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data TimeExtension = MkTimeExtension
  { window :: Positive
  -- ^ A period of time (in ms) BEFORE auction.time.close in
  --    which it's possible to extend the auction.
  , length :: Positive
  -- ^ A period of time (in ms) FROM auction.time.close on
  --    which auction can be extended.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data AuctionInfo = MkAuctionInfo
  { seller :: Address
  , beneficiaries :: Map Address Positive
  -- ^ A map from beneficiary to her winning bid part, in percentage.
  --    Sum of all parts must be equal to 100.
  , sellerToCoverFees :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data BidInfo = MkBidInfo
  { buyNowPrice :: Positive
  -- ^ ADA
  , startingPrice :: Positive
  -- ^ ADA
  , raiseMinimum :: Positive
  -- ^ ADA
  , raisePercentage :: Positive
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data MarketTerms = MkMarketTerms
  { fixedFee :: Integer
  -- ^ Flat fixed fee
  , percentageFee :: Integer
  -- ^ Fee percentage of the winning Bid
  , feeAddress :: Address
  -- ^ The Address the fees are to be paid to.
  , minAda :: Positive
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data RegistrationEscrow = MkRegistrationEscrow
  { registrant :: Address
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data AuctionEscrow = MkAuctionEscrow
  { terms :: AuctionTerms
  , nodeCS :: CurrencySymbol
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data BidEscrow = MkBidEscrow
  { status :: BidStatus
  , bidder :: Address
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data BidStatus
  = NoBid
  | Bid Positive POSIXTime
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data RefundReason = LostBid | BoughtLot
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data CancelReason = Failed | Bought
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data PrivAuctionRedeemer
  = CancelAct CurrencySymbol CancelReason
  | BidAct CurrencySymbol
  | ExtendAct CurrencySymbol
  | CloseExtensionsAct CurrencySymbol
  | BuyNowAct CurrencySymbol
  | RefundAct CurrencySymbol RefundReason
  | ResolveAct CurrencySymbol
  | UnregisterAct CurrencySymbol
  | IssuePaddleAct CurrencySymbol
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data PubAuctionRedeemer
  = PubCancelAct CurrencySymbol CancelReason
  | PubBidAct CurrencySymbol
  | PubExtendAct CurrencySymbol
  | PubCloseExtensionsAct CurrencySymbol
  | PubBuyNowAct CurrencySymbol
  | PubBuyNowNoEnrollAct CurrencySymbol Address
  | PubRefundAct CurrencySymbol RefundReason
  | PubResolveAct CurrencySymbol
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data PrivStateMintingRedeemer
  = AnnounceAuction
  | Registration
  | AuctionLifeCycle
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data PubStateMintingRedeemer
  = PubAnnounceAuction
  | PubEnrollBidder
  | PubAuctionLifeCycle
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

unstableMakeIsData ''BidStatus
unstableMakeIsData ''TimeExtension
unstableMakeIsData ''AuctionInfo
unstableMakeIsData ''AuctionTime
unstableMakeIsData ''BidInfo
unstableMakeIsData ''AuctionTerms
unstableMakeIsData ''AuctionEscrow
unstableMakeIsData ''RegistrationEscrow
unstableMakeIsData ''BidEscrow
unstableMakeIsData ''RefundReason
unstableMakeIsData ''CancelReason
unstableMakeIsData ''MarketTerms
unstableMakeIsData ''PubAuctionRedeemer
unstableMakeIsData ''PrivAuctionRedeemer
unstableMakeIsData ''PrivStateMintingRedeemer
unstableMakeIsData ''PubStateMintingRedeemer
