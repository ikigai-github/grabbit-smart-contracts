module Test.Vulcan.Unit.Auction.Private (spec, testResult) where

import Test.Hspec (Spec, describe)

import Plutarch.Api.V2 (PValidator)
import Plutarch.Test (
  GoldenConf,
  pfails,
  pgoldenSpec',
  psucceeds,
  (@->),
  (@\),
  (@|),
 )

import PlutusLedgerApi.V1.Interval (from, to)
import PlutusLedgerApi.V2 (
  Data (Constr),
  ScriptContext,
  always,
  singleton,
 )
import PlutusTx qualified as Plutus

import Plutarch.Context (
  SpendingBuilder,
  buildSpending,
  input,
  mint,
  mkNormalized,
  output,
  pubKey,
  referenceInput,
  script,
  signedWith,
  timeRange,
  withInlineDatum,
  withRef,
  withSpendingOutRef,
  withValue,
 )
import Test.Vulcan.CommonInputs (
  auctionDatum,
  bid,
  change,
  checkVulcan,
  closeTime,
  corrNodeToken,
  doubleMinAdaVal,
  exampleMarket,
  fees,
  halfPrice,
  higherBid,
  lotVal,
  marketCS,
  marketPK,
  marketToken,
  minAdaVal,
  mkD,
  nodeToken,
  otherValHash,
  pk,
  pk1,
  pk2,
  privAuctionToken,
  privBidToken,
  privateAuctionHash,
  privateStateCS,
  ref1,
  regToken,
  registration,
  startingVal,
 )

import Test.Vulcan.Unit.Auction.Private.BidAct qualified as BidAct
import Test.Vulcan.Unit.Auction.Private.BuyNowAct qualified as BuyNowAct
import Test.Vulcan.Unit.Auction.Private.CancelAct qualified as CancelAct
import Test.Vulcan.Unit.Auction.Private.ExtendAct qualified as ExtendAct
import Test.Vulcan.Unit.Auction.Private.IssuePaddleAct qualified as IssuePaddleAct
import Test.Vulcan.Unit.Auction.Private.RefundAct qualified as RefundAct
import Test.Vulcan.Unit.Auction.Private.ResolveAct qualified as ResolveAct
import Test.Vulcan.Unit.Auction.Private.UnregisterAct qualified as UnregisterAct
import Test.Vulcan.Unit.Debug (EvalResult, showEvalResult)
import Vulcan.Common.Types.Auction (
  CancelReason (Bought, Failed),
  PrivAuctionRedeemer (
    BidAct,
    BuyNowAct,
    CancelAct,
    ExtendAct,
    IssuePaddleAct,
    RefundAct,
    ResolveAct,
    UnregisterAct
  ),
  RefundReason (BoughtLot, LostBid),
 )
import Vulcan.Common.Types.Instances (Config (Config), TracingMode (DoTracing), Verbosity (Verbose), tracingMode)
import Vulcan.Onchain.Auction.Validator.Private (mkPrivateAuctionValidator)
import Vulcan.Utils.Scripts (wrapValidator')

testResult :: EvalResult
testResult =
  let redm = ResolveAct privateStateCS
      ctx =
        ResolveAct.mkCtx
          . ResolveAct.mustPayMarketFees
          $ ResolveAct.whenBidderPayingFees
      term = target # dat # pconstant (Plutus.toData redm) # pconstant ctx
   in showEvalResult term

spec :: GoldenConf -> Spec
spec goldenConf = describe "Private Auction Validator" . pgoldenSpec' goldenConf $ do
  ---------------------------------------------
  -- Success
  ---------------------------------------------
  "Correct" @\ do
    "Succesful CancelAct Transaction on Failed auction"
      @| let redm = CancelAct privateStateCS Failed
             ctx = CancelAct.mkCtx CancelAct.failedAuction
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful CancelAct Transaction on auction after lot has been bought"
      @| let redm = CancelAct privateStateCS Bought
             ctx = CancelAct.mkCtx CancelAct.postBougthAuction
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful UnregisterAct Transaction"
      @| let redm = UnregisterAct privateStateCS
             ctx = UnregisterAct.mkCtx UnregisterAct.succeeds
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful IssuePaddleAct Transaction"
      @| let redm = IssuePaddleAct privateStateCS
             ctx = IssuePaddleAct.mkCtx IssuePaddleAct.succeeds
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful BidAct Transaction"
      @| let redm = BidAct privateStateCS
             ctx = BidAct.mkCtx BidAct.succeeds
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful ExtendAct Transaction"
      @| let redm = ExtendAct privateStateCS
             ctx = ExtendAct.mkCtx ExtendAct.succeeds
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful BuyNowAct Transaction"
      @| let redm = BuyNowAct privateStateCS
             ctx = BuyNowAct.mkCtx BuyNowAct.succeeds
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful RefundAct Transaction" @\ do
      "Refunds Lost empty bid after auction"
        @| let redm = RefundAct privateStateCS LostBid
               ctx = RefundAct.mkCtx RefundAct.whenNoBidAfterAuction
            in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

      {- We do not enforce the SetNode minAda to be refunded to the bidder as this is
           used to pay Tx fees and as an incentive to anyone calling a refund
        -}
      "Refunds Lost empty bid after auction, one min ADA less"
        @| let redm = RefundAct privateStateCS LostBid
               ctx =
                RefundAct.mkCtx
                  . RefundAct.refundOneMinAdaLess
                  $ RefundAct.whenNoBidAfterAuction
            in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

      "Refunds Lost empty bid during auction when signed by the bidder"
        @| let redm = RefundAct privateStateCS LostBid
               ctx =
                RefundAct.mkCtx
                  . RefundAct.duringAuctionWhenAuthorized
                  $ RefundAct.whenNoBidAfterAuction
            in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

      "Refunds bid against higher after auction"
        @| let redm = RefundAct privateStateCS LostBid
               ctx = RefundAct.mkCtx RefundAct.againstHigherAfterAuction
            in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

      "Refunds bid against higher during auction when signed by the bidder"
        @| let redm = RefundAct privateStateCS LostBid
               ctx =
                RefundAct.mkCtx
                  . RefundAct.duringAuctionWhenAuthorized
                  $ RefundAct.againstHigherAfterAuction
            in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

      "Refunds empty bid after BuyNow"
        @| let redm = RefundAct privateStateCS BoughtLot
               ctx = RefundAct.mkCtx RefundAct.noBidWhenLotBought
            in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

      "Refunds non-empty bid after BuyNow"
        @| let redm = RefundAct privateStateCS BoughtLot
               ctx =
                RefundAct.mkCtx
                  . RefundAct.theBid
                  $ RefundAct.noBidWhenLotBought
            in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds
    "Succesful ResolveAct Transaction" @\ do
      "When Seller covers fees"
        @| let redm = ResolveAct privateStateCS
               ctx = ResolveAct.mkCtx ResolveAct.whenSellerPayingFees
            in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds
      "When Winner covers fees"
        @| let redm = ResolveAct privateStateCS
               ctx = ResolveAct.mkCtx ResolveAct.whenBidderPayingFees
            in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds
  ---------------------------------------------
  -- Failure
  ---------------------------------------------
  "Incorrect" @\ do
    "Failure: CancelAct.Bought: Mints extra"
      @| let redm = CancelAct privateStateCS Bought
             ctx = CancelAct.mkCtx $ CancelAct.cantMintExtra CancelAct.postBougthAuction
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: CancelAct.Failed: Mints extra"
      @| let redm = CancelAct privateStateCS Failed
             ctx = CancelAct.mkCtx $ CancelAct.cantMintExtra CancelAct.failedAuction
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: CancelAct.Bought: Spends extra from AuctionValidator"
      @| let redm = CancelAct privateStateCS Bought
             ctx = CancelAct.mkCtx $ CancelAct.cantSpendExtraFromAuction CancelAct.postBougthAuction
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: CancelAct.Failed: Spends extra from AuctionValidator"
      @| let redm = CancelAct privateStateCS Failed
             ctx = CancelAct.mkCtx $ CancelAct.cantSpendExtraFromAuction CancelAct.failedAuction
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: CancelAct.Bought: Allows to steal seller refund"
      @| let redm = CancelAct privateStateCS Bought
             ctx = CancelAct.mkCtx $ CancelAct.cantStealSellerRefund CancelAct.postBougthAuction
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: CancelAct.Failed: Allows to steal seller refund"
      @| let redm = CancelAct privateStateCS Failed
             ctx = CancelAct.mkCtx $ CancelAct.cantStealSellerRefund CancelAct.failedAuction
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: CancelAct.Failed: Doesn't require seller signature"
      @| let redm = CancelAct privateStateCS Failed
             ctx = CancelAct.mkCtx CancelAct.mustBeAuthorizedIfLotPresent
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: UnregisterAct: Mints extra"
      @| let redm = UnregisterAct privateStateCS
             ctx = UnregisterAct.mkCtx UnregisterAct.shouldntMintExtra
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: UnregisterAct: Does not return all funds"
      @| let redm = UnregisterAct privateStateCS
             ctx = UnregisterAct.mkCtx UnregisterAct.shouldRefundAllFunds
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: IssuePaddleAct mints extra"
      @| let redm = IssuePaddleAct privateStateCS
             ctx = IssuePaddleAct.mkCtx IssuePaddleAct.mustNotMintExtra
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: IssuePaddleAct does't mint BidEscrow token"
      @| let redm = IssuePaddleAct privateStateCS
             ctx = IssuePaddleAct.mkCtx IssuePaddleAct.mustMintBidToken
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: ExtendAct mints"
      @| let redm = ExtendAct privateStateCS
             ctx = ExtendAct.mkCtx ExtendAct.cantMint
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: ExtendAct doesn't change AuctionEscrow UTxO"
      @| let redm = ExtendAct privateStateCS
             ctx = ExtendAct.mkCtx ExtendAct.mustChangeAuction
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: ExtendAct spend extra from AuctionValidator"
      @| let redm = ExtendAct privateStateCS
             ctx = ExtendAct.mkCtx ExtendAct.doesntSpendExtraFromValidator
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: ExtendAct bid time is before extension window"
      @| let redm = ExtendAct privateStateCS
             ctx = ExtendAct.mkCtx ExtendAct.bidTimeMustBeInExtensioWindow
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: BuyNowAct mints extra"
      @| let redm = BuyNowAct privateStateCS
             ctx = BuyNowAct.mkCtx BuyNowAct.cantMintExtra
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: BuyNowAct mints more than 1 BoughtToken"
      @| let redm = BuyNowAct privateStateCS
             ctx = BuyNowAct.mkCtx BuyNowAct.cantMintMoreBoughtTokens
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: BuyNowAct doesn't change AuctionEscrow UTxO"
      @| let redm = BuyNowAct privateStateCS
             ctx = BuyNowAct.mkCtx BuyNowAct.mustChangeAuction
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: BuyNowAct doesn't reward winner"
      @| let redm = BuyNowAct privateStateCS
             ctx = BuyNowAct.mkCtx BuyNowAct.mustRewardWinner
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure BidAct Mints some other token"
      @| let redm = BidAct privateStateCS
             ctx = BidAct.mkCtx BidAct.mustNotMint
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure BidAct Mints node token"
      @| let redm = BidAct privateStateCS
             ctx = BidAct.mkCtx BidAct.mustNotMintNode
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure BidAct bid time must fit in Tx validity range"
      @| let redm = BidAct privateStateCS
             ctx = BidAct.mkCtx BidAct.bidMustBeAfterValidityRange
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure RefundAct" @\ do
      let testNegative reason againstInvalidCtx =
            let redm = RefundAct privateStateCS reason
                ctx = RefundAct.mkCtx againstInvalidCtx
             in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails
      "Mints extra"
        @| testNegative LostBid
        $ RefundAct.mustNotMintExtra RefundAct.againstHigherAfterAuction
      "Refunds against higher bid, with no min ADA"
        @| testNegative LostBid
        $ RefundAct.mustRefundMinAda RefundAct.againstHigherAfterAuction
      "Refunds empty bid, with no min ADA"
        @| testNegative LostBid
        $ RefundAct.mustRefundMinAda RefundAct.whenNoBidAfterAuction
      "Refunds non-empty bid, with no min ADA"
        @| testNegative LostBid
          . RefundAct.mustRefundMinAda
        $ RefundAct.theBid RefundAct.whenNoBidAfterAuction
      "Doesn't require bidder signature during auction when lot present"
        @| testNegative BoughtLot
        $ RefundAct.buyNowRefundMustNotWorkWhenLotPresent
          RefundAct.noBidWhenLotBought
      "Refunds against higher bid, but to incorrect bidder"
        @| testNegative LostBid
        $ RefundAct.mustRefundCorrectBidder
          RefundAct.againstHigherAfterAuction
      "Refunds empty bid, but to incorrect bidder"
        @| testNegative LostBid
        $ RefundAct.mustRefundCorrectBidder
          RefundAct.whenNoBidAfterAuction
      "Refunds against higher bid during auction without bidder signature"
        @| testNegative LostBid
        $ RefundAct.mustNotRefundBidDuringAuctionUnauthorized
          RefundAct.againstHigherAfterAuction
      "Refunds empty bid during auction without bidder signature"
        @| testNegative LostBid
        $ RefundAct.mustNotRefundBidDuringAuctionUnauthorized
          RefundAct.whenNoBidAfterAuction

    "Failure ResolveAct" @\ do
      let testNegative againstInvalidCtx =
            let redm = ResolveAct privateStateCS
                ctx = ResolveAct.mkCtx againstInvalidCtx
             in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails
      "Doesn't burn Auction"
        @| testNegative
        $ ResolveAct.mustBurnAuction ResolveAct.whenSellerPayingFees

      "Incorrectly pays benificiaries"
        @| testNegative
        $ ResolveAct.mustCorrectlyPayBeneficiaries
          ResolveAct.whenBidderPayingFees

      "Doesn't pay market fees"
        @| testNegative
        $ ResolveAct.mustPayMarketFees
          ResolveAct.whenBidderPayingFees

----------------------------------
-- Target Validator

dat :: Term s PData
dat = pconstant $ Constr 0 []

target :: ClosedTerm PValidator
target = wrapValidator' $ mkPrivateAuctionValidator Config {tracingMode = DoTracing Verbose} marketCS
