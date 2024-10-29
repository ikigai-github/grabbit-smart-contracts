module Test.Vulcan.Unit.StateMP.Public (spec) where

import Test.Hspec (Spec, describe)

import Plutarch.Api.V2 (PMintingPolicy)
import Plutarch.Test (
  GoldenConf,
  pfails,
  pgoldenSpec',
  psucceeds,
  (@->),
  (@\),
  (@|),
 )

import PlutusLedgerApi.V2 (ScriptContext, always, from, to)
import PlutusTx qualified as Plutus

import Vulcan.Onchain.Auction.StateMP.Public (mkPubStateMP)
import Vulcan.Utils.Scripts (wrapMintingPolicy')

import Vulcan.Common.Types.Auction (
  PubStateMintingRedeemer (..),
 )

import Vulcan.Common.Types.Instances (Config (Config), TracingMode (..), Verbosity (..), tracingMode)

import Plutarch.Context
import PlutusLedgerApi.V1.Interval (interval)
import Test.Vulcan.CommonInputs

spec :: GoldenConf -> Spec
spec goldenConf = describe "State Minting Policy" . pgoldenSpec' goldenConf $ do
  ---------------------------------------------
  -- Success
  ---------------------------------------------
  "Correct" @\ do
    "Succesful AnnounceAuction Transaction"
      @| let redm = PubAnnounceAuction
             ctx = announceCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful Enroll Transaction"
      @| let redm = PubEnrollBidder
             ctx = enrollCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful Enroll Transaction with bid"
      @| let redm = PubEnrollBidder
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ enrollCtxBuilder
                    , output $
                        script publicAuctionHash
                          <> withValue (minAdaVal <> startingVal <> pubBidToken 1)
                          <> withInlineDatum (mkD raisedBid)
                    , timeRange $ interval (startTime + 1) (getBidTime raisedBid - 1)
                    , mint $ pubBidToken 1
                    , mint $ nodeToken (Just pk1) 1
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful LifecycleTransaction"
      @| let redm = PubAuctionLifeCycle
             ctx = lifecycleCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    ---------------------------------------------
    -- Failure
    ---------------------------------------------

    "Failure: AnnounceAuction Transaction mints wrong token"
      @| let redm = PubAnnounceAuction
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ announceCtxBuilder
                    , output $
                        script publicAuctionHash
                          <> withValue (doubleMinAdaVal <> lotVal <> pubAuctionToken 1 <> corrNodeToken 1)
                          <> withInlineDatum auctionDatum
                    , timeRange always
                    , mint $ nodeToken Nothing 1
                    , mint $ pubBidToken 1
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: AnnounceAuction Transaction AuctionEscrow State Token has the wrong CS"
      @| let redm = PubAnnounceAuction
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ announceCtxBuilder
                    , output $
                        script publicAuctionHash
                          <> withValue (doubleMinAdaVal <> lotVal <> incorrectAuctionToken <> corrNodeToken 1)
                          <> withInlineDatum auctionDatum
                    , timeRange $ to (startTime - 1)
                    , mint $ pubAuctionToken 1
                    , mint $ corrNodeToken 1 <> nodeToken Nothing 1
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Enroll Transaction with bid prior to start"
      @| let redm = PubEnrollBidder
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ enrollCtxBuilder
                    , output $
                        script publicAuctionHash
                          <> withValue (startingVal <> minAdaVal <> pubBidToken 1)
                          <> withInlineDatum (mkD raisedBid)
                    , mint $ pubBidToken 1
                    , mint $ nodeToken (Just pk1) 1
                    , timeRange $ to (closeTime - 1)
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Enroll Transaction with incorrect node Token"
      @| let redm = PubEnrollBidder
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ enrollCtxBuilder
                    , output $
                        script publicAuctionHash
                          <> withValue (startingVal <> minAdaVal <> pubBidToken 1)
                          <> withInlineDatum (mkD bid)
                    , mint $ pubBidToken 1
                    , mint $ nodeToken (Just pk) 1
                    , timeRange $ to (closeTime - 1)
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

----------------------------------
-- Target Validator

target :: ClosedTerm PMintingPolicy
target =
  wrapMintingPolicy' $
    mkPubStateMP (Config {tracingMode = DoTracing Verbose}) publicAuctionHash marketCS
      # pconstant finSetCS
      # pconstant correctRef

----------------------------------
-- Correct Contexts

announceCtxBuilder :: MintingBuilder
announceCtxBuilder =
  mconcat
    [ withMinting publicStateCS
    , referenceInput $
        script otherValHash
          <> withValue (minAdaVal <> marketToken)
          <> withInlineDatum (mkD exampleMarket)
          <> withRef ref2
    , input $
        pubKey pk
          <> withValue (minAdaVal <> minAdaVal <> lotVal)
          <> withRef correctRef
    , signedWith pk
    , timeRange always
    ]

announceCtx :: ScriptContext
announceCtx =
  let builder :: MintingBuilder
      builder =
        announceCtxBuilder
          <> mconcat
            [ output $
                script publicAuctionHash
                  <> withValue (doubleMinAdaVal <> lotVal <> pubAuctionToken 1 <> corrNodeToken 1)
                  <> withInlineDatum auctionDatum
            , mint $ pubAuctionToken 1
            , mint $ corrNodeToken 1 <> nodeToken Nothing 1
            ]
   in buildMinting [checkMinting] $ mkNormalized builder

enrollCtxBuilder :: MintingBuilder
enrollCtxBuilder =
  mconcat
    [ withMinting publicStateCS
    , referenceInput $
        script otherValHash
          <> withValue (minAdaVal <> marketToken)
          <> withInlineDatum (mkD exampleMarket)
    , referenceInput $
        script publicAuctionHash
          <> withValue (doubleMinAdaVal <> lotVal <> pubAuctionToken 1 <> corrNodeToken 1)
          <> withInlineDatum auctionDatum
    , input $
        pubKey pk1
          <> withValue (startingVal <> minAdaVal)
    , signedWith pk1
    ]

enrollCtx :: ScriptContext
enrollCtx =
  let builder :: MintingBuilder
      builder =
        enrollCtxBuilder
          <> mconcat
            [ output $
                script publicAuctionHash
                  <> withValue (startingVal <> minAdaVal <> pubBidToken 1)
                  <> withInlineDatum (mkD bid)
            , mint $ pubBidToken 1
            , mint $ nodeToken (Just pk1) 1
            , timeRange (to (closeTime - 1))
            ]
   in buildMinting checkVulcan $ mkNormalized builder

lifecycleCtx :: ScriptContext
lifecycleCtx =
  let builder :: MintingBuilder
      builder =
        mconcat
          [ withMinting publicStateCS
          , referenceInput $
              script otherValHash
                <> withValue (minAdaVal <> marketToken)
                <> withInlineDatum (mkD exampleMarket)
                <> withRef ref2
          , input $
              script publicAuctionHash
                <> withValue (doubleMinAdaVal <> lotVal <> pubAuctionToken 1)
                <> withInlineDatum auctionDatum
                <> withRef ref1
          , timeRange always
          , mint $ pubBidToken (-1)
          ]
   in buildMinting checkVulcan $ mkNormalized builder
