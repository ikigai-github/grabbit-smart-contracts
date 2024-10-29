module Test.Vulcan.Unit.FinSet.MP (
  spec,
  insertCtx,
  insertCtxBuilder,
  removeCtx,
  removeCtxBuilder,
  initCtx,
  initCtxBuilder,
  removedeinitCtx,
  removedeinitCtxBuilder,
  deinitCtx,
  deinitCtxBuilder,
) where

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

import PlutusLedgerApi.V1.Interval (always)
import PlutusLedgerApi.V2 (ScriptContext)
import PlutusTx qualified as Plutus

import Vulcan.Common.Types.FinSet (
  NodeAction (Deinit, Init, Insert, Remove, RemoveAndDeinit),
 )
import Vulcan.Common.Types.Instances (Config (Config), TracingMode (..), Verbosity (..), tracingMode)
import Vulcan.Onchain.FinSet.MP.Standard (mkNodeMP)
import Vulcan.Utils.Scripts (wrapMintingPolicy')

import Plutarch.Context
import Test.Vulcan.CommonInputs (
  auctionDatum,
  checkVulcan,
  corrNodeToken,
  correctRef,
  emptyNode,
  exampleMarket,
  finSetCS,
  finSetValHash,
  firstNode,
  insertNode,
  lastNode,
  marketCS,
  marketToken,
  minAdaVal,
  mkD,
  nodeToken,
  otherValHash,
  pk,
  pk1,
  privAuctionToken,
  privBidToken,
  privateAuctionHash,
  ref1,
  ref2,
  updatedNode,
 )
import Test.Vulcan.Unit.Debug (EvalResult, showEvalResult)

spec :: GoldenConf -> Spec
spec goldenConf = describe "FinSet Minting Policy" . pgoldenSpec' goldenConf $ do
  ---------------------------------------------
  -- Success
  ---------------------------------------------
  "Correct" @\ do
    -- Success Case :
    "Succesful Init transaction"
      @| let redm = Init
             ctx = initCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful DeInit transaction"
      @| let redm = Deinit
             ctx = deinitCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful Insert transaction"
      @| let redm = Insert pk firstNode
             ctx = insertCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful Remove transaction"
      @| let redm = Remove pk firstNode
             ctx = removeCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful RemoveAndDeinit transaction"
      @| let redm = RemoveAndDeinit pk1
             ctx = removedeinitCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

  ----------------------------------------------------
  -- Failure:
  ----------------------------------------------------
  "Incorrect" @\ do
    -- Failure Case :
    "Failure: Init mints incorrect Value"
      @| let redm = Init
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ initCtxBuilder
                    , input $
                        script otherValHash
                          <> withRef correctRef
                          <> withValue minAdaVal
                    , output $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken Nothing 1)
                          <> withInlineDatum (mkD emptyNode)
                    , mint $ nodeToken Nothing (-1)
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Init does not consume correct TxOutRef"
      @| let redm = Init
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ initCtxBuilder
                    , input $
                        script otherValHash
                          <> withRef ref1
                          <> withValue minAdaVal
                    , output $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken Nothing 1)
                          <> withInlineDatum (mkD emptyNode)
                    , mint $ nodeToken Nothing 1 <> corrNodeToken 1
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Init contains the incorrect datum out"
      @| let redm = Init
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ initCtxBuilder
                    , input $
                        script otherValHash
                          <> withRef correctRef
                          <> withValue minAdaVal
                    , output $
                        script finSetValHash
                          <> withValue (nodeToken (Just pk1) 1 <> minAdaVal)
                          <> withInlineDatum (mkD lastNode)
                    , mint $ nodeToken Nothing 1 <> corrNodeToken 1
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Deinit does not send to Node Validator"
      @| let redm = Deinit
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ deinitCtxBuilder
                    , input $
                        script otherValHash
                          <> withValue (nodeToken Nothing 1 <> minAdaVal)
                          <> withInlineDatum (mkD emptyNode)
                          <> withRef correctRef
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Deinit has node output"
      @| let redm = Deinit
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ deinitCtxBuilder
                    , input $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken Nothing 1)
                          <> withInlineDatum (mkD emptyNode)
                    , input $
                        script privateAuctionHash
                          <> withValue (minAdaVal <> privAuctionToken 1 <> corrNodeToken 1)
                          <> withInlineDatum auctionDatum
                    , output $
                        script finSetValHash
                          <> withValue (nodeToken Nothing 1 <> minAdaVal)
                          <> withInlineDatum (mkD emptyNode)
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Insert transaction does not have token in outputs"
      @| let redm = Insert pk firstNode
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ insertCtxBuilder
                    , output $
                        script finSetValHash
                          <> withValue (nodeToken (Just pk) 1 <> minAdaVal)
                          <> withInlineDatum (mkD insertNode)
                    , output $
                        script finSetValHash
                          <> withValue minAdaVal
                          <> withInlineDatum insertNode
                    , mint $ privBidToken 1 <> nodeToken (Just pk) 1
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Insert tries to mint wrong node Token"
      @| let redm = Insert pk firstNode
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ insertCtxBuilder
                    , output $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken (Just pk) 1)
                          <> withInlineDatum (mkD insertNode)
                    , output $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken Nothing 1)
                          <> withInlineDatum (mkD updatedNode)
                    , mint $ nodeToken (Just pk1) 1
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Insert tries to remove transaction"
      @| let redm = Insert pk firstNode
             ctx = removeCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Remove tries to burn wrong node Token"
      @| let redm = Remove pk lastNode
             ctx = buildMinting checkVulcan $ mkNormalized $ removeCtxBuilder <> mint (nodeToken Nothing (-1))
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Remove tries to Deinit transaction"
      @| let redm = Remove pk lastNode
             ctx = deinitCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: RemoveAndDeinit mints incorrect amount"
      @| let redm = RemoveAndDeinit pk1
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ removedeinitCtxBuilder
                    , mint $ nodeToken Nothing (-1)
                    , input $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken (Just pk1) 1)
                          <> withInlineDatum (mkD lastNode)
                          <> withRef ref1
                    , input $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken (Just pk1) 1)
                          <> withInlineDatum (mkD lastNode)
                          <> withRef ref1
                    , input $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken Nothing 1)
                          <> withInlineDatum (mkD firstNode)
                          <> withRef ref1
                    , input $
                        script privateAuctionHash
                          <> withValue (minAdaVal <> corrNodeToken 1 <> privAuctionToken 1)
                          <> withInlineDatum auctionDatum
                          <> withRef ref2
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: RemoveAndDeinit does not have input from auction Validator"
      @| let redm = RemoveAndDeinit pk1
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ removedeinitCtxBuilder
                    , mint $ privAuctionToken (-1) <> privBidToken (-1)
                    , mint $ nodeToken Nothing (-1) <> nodeToken (Just pk1) (-1) <> corrNodeToken (-1)
                    , input $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken (Just pk1) 1)
                          <> withInlineDatum (mkD lastNode)
                          <> withRef ref1
                    , input $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken Nothing 1)
                          <> withInlineDatum (mkD firstNode)
                          <> withRef ref1
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

----------------------------------
-- Target Validator:

target :: ClosedTerm PMintingPolicy
target =
  wrapMintingPolicy' $
    mkNodeMP Config {tracingMode = DoTracing Verbose} marketCS finSetValHash
      # pconstant correctRef

----------------------------------
-- Correct Contexts

initCtxBuilder :: MintingBuilder
initCtxBuilder =
  mconcat
    [ withMinting finSetCS
    , referenceInput $
        script otherValHash
          <> withInlineDatum (mkD exampleMarket)
          <> withValue (minAdaVal <> marketToken)
          <> withRef ref1
    , timeRange always
    ]

initCtx :: ScriptContext
initCtx =
  let builder :: MintingBuilder
      builder =
        mconcat
          [ withMinting finSetCS
          , referenceInput $
              script otherValHash
                <> withInlineDatum (mkD exampleMarket)
                <> withValue (minAdaVal <> marketToken)
                <> withRef ref1
          , input $
              script otherValHash
                <> withRef correctRef
                <> withValue minAdaVal
          , output $
              script finSetValHash
                <> withValue (minAdaVal <> nodeToken Nothing 1)
                <> withInlineDatum (mkD emptyNode)
          , timeRange always
          , mint (nodeToken Nothing 1 <> corrNodeToken 1)
          ]
   in buildMinting checkVulcan (mkNormalized builder)

deinitCtxBuilder :: MintingBuilder
deinitCtxBuilder =
  mconcat
    [ withMinting finSetCS
    , referenceInput $
        script otherValHash
          <> withValue (minAdaVal <> marketToken)
          <> withInlineDatum (mkD exampleMarket)
          <> withRef ref2
    , output $
        pubKey "deadbeefdeadbeefdeadbeef"
          <> withValue (minAdaVal <> minAdaVal)
    , mint (privAuctionToken (-1) <> nodeToken Nothing (-1) <> corrNodeToken (-1))
    , timeRange always
    ]

deinitCtx :: ScriptContext
deinitCtx =
  let builder :: MintingBuilder
      builder =
        mconcat
          [ withMinting finSetCS
          , referenceInput $
              script otherValHash
                <> withValue (minAdaVal <> marketToken)
                <> withInlineDatum (mkD exampleMarket)
                <> withRef ref2
          , input $
              script finSetValHash
                <> withValue (minAdaVal <> nodeToken Nothing 1)
                <> withInlineDatum (mkD emptyNode)
          , input $
              script privateAuctionHash
                <> withValue (minAdaVal <> privAuctionToken 1 <> corrNodeToken 1)
                <> withInlineDatum auctionDatum
          , output $
              pubKey "deadbeefdeadbeefdeadbeef"
                <> withValue (minAdaVal <> minAdaVal)
          , mint (privAuctionToken (-1) <> nodeToken Nothing (-1) <> corrNodeToken (-1))
          , timeRange always
          ]
   in buildMinting checkVulcan (mkNormalized builder)

insertCtx :: ScriptContext
insertCtx =
  let builder :: MintingBuilder
      builder =
        mconcat
          [ withMinting finSetCS
          , referenceInput $
              script otherValHash
                <> withValue (minAdaVal <> marketToken)
                <> withInlineDatum (mkD exampleMarket)
                <> withRef ref2
          , referenceInput $
              script privateAuctionHash
                <> withValue (minAdaVal <> corrNodeToken 1 <> privAuctionToken 1)
                <> withInlineDatum auctionDatum
                <> withRef ref2
          , input $
              script finSetValHash
                <> withValue (minAdaVal <> nodeToken Nothing 1)
                <> withInlineDatum (mkD firstNode)
                <> withRef ref1
          , input $
              script privateAuctionHash
                <> withValue minAdaVal
                <> withRef ref2
          , output $
              script finSetValHash
                <> withValue (minAdaVal <> nodeToken (Just pk) 1)
                <> withInlineDatum (mkD insertNode)
          , output $
              script finSetValHash
                <> withValue (minAdaVal <> nodeToken Nothing 1)
                <> withInlineDatum (mkD updatedNode)
          , mint (privBidToken 1 <> nodeToken (Just pk) 1)
          , timeRange always
          ]
   in buildMinting checkVulcan (mkNormalized builder)

insertCtxBuilder :: MintingBuilder
insertCtxBuilder =
  mconcat
    [ withMinting finSetCS
    , referenceInput $
        script otherValHash
          <> withValue (minAdaVal <> marketToken)
          <> withInlineDatum (mkD exampleMarket)
          <> withRef ref2
    , referenceInput $
        script privateAuctionHash
          <> withValue (minAdaVal <> corrNodeToken 1 <> privAuctionToken 1)
          <> withInlineDatum auctionDatum
          <> withRef ref2
    , input $
        script finSetValHash
          <> withValue (minAdaVal <> nodeToken Nothing 1)
          <> withInlineDatum (mkD firstNode)
          <> withRef ref1
    , input $
        script privateAuctionHash
          <> withValue minAdaVal
          <> withRef ref2
    , timeRange always
    ]

removeCtx :: ScriptContext
removeCtx =
  let builder :: MintingBuilder
      builder =
        mconcat
          [ referenceInput $
              script otherValHash
                <> withValue (minAdaVal <> marketToken)
                <> withInlineDatum (mkD exampleMarket)
          , referenceInput $
              script privateAuctionHash
                <> withValue (minAdaVal <> corrNodeToken 1 <> privAuctionToken 1)
                <> withInlineDatum auctionDatum
          , input $
              mconcat
                [ script finSetValHash
                , withValue (nodeToken (Just pk) 1 <> minAdaVal)
                , withInlineDatum (mkD insertNode)
                ]
          , input $
              script finSetValHash
                <> withValue (minAdaVal <> nodeToken Nothing 1)
                <> withInlineDatum (mkD updatedNode)
          , mint (privBidToken (-1) <> nodeToken (Just pk) (-1))
          , output $
              mconcat
                [ script finSetValHash
                , withValue (minAdaVal <> nodeToken Nothing 1)
                , withInlineDatum (mkD firstNode)
                ]
          , withMinting finSetCS
          , timeRange always
          ]
   in buildMinting checkVulcan (mkNormalized builder)

removeCtxBuilder :: MintingBuilder
removeCtxBuilder =
  mconcat
    [ referenceInput $
        script otherValHash
          <> withValue (minAdaVal <> marketToken)
          <> withInlineDatum (mkD exampleMarket)
    , referenceInput $
        script privateAuctionHash
          <> withValue (minAdaVal <> corrNodeToken 1 <> privAuctionToken 1)
          <> withInlineDatum auctionDatum
    , withMinting finSetCS
    , timeRange always
    ]

removedeinitCtx :: ScriptContext
removedeinitCtx =
  let builder :: MintingBuilder
      builder =
        mconcat
          [ withMinting finSetCS
          , referenceInput $
              script otherValHash
                <> withValue (minAdaVal <> marketToken)
                <> withInlineDatum (mkD exampleMarket)
                <> withRef ref1
          , input $
              script finSetValHash
                <> withValue (minAdaVal <> nodeToken (Just pk1) 1)
                <> withInlineDatum (mkD lastNode)
                <> withRef ref1
          , input $
              script finSetValHash
                <> withValue (minAdaVal <> nodeToken Nothing 1)
                <> withInlineDatum (mkD firstNode)
                <> withRef ref1
          , input $
              script privateAuctionHash
                <> withValue (minAdaVal <> corrNodeToken 1 <> privAuctionToken 1)
                <> withInlineDatum auctionDatum
                <> withRef ref2
          , output $
              pubKey "deadbeefdeadbeefdeadbeef"
                <> withValue (minAdaVal <> minAdaVal <> minAdaVal)
          , mint (privAuctionToken (-1) <> privBidToken (-1) <> nodeToken Nothing (-1) <> nodeToken (Just pk1) (-1) <> corrNodeToken (-1))
          , timeRange always
          ]
   in buildMinting checkVulcan (mkNormalized builder)

removedeinitCtxBuilder :: MintingBuilder
removedeinitCtxBuilder =
  mconcat
    [ withMinting finSetCS
    , referenceInput $
        script otherValHash
          <> withValue (minAdaVal <> marketToken)
          <> withInlineDatum (mkD exampleMarket)
          <> withRef ref1
    , output $
        pubKey "deadbeefdeadbeefdeadbeef"
          <> withValue (minAdaVal <> minAdaVal <> minAdaVal)
    , timeRange always
    ]
