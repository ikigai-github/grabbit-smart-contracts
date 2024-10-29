module Test.Vulcan.Unit.FinSet.Separator (spec) where

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

import PlutusLedgerApi.V1.Interval (always, from, to)
import PlutusLedgerApi.V2 (PubKeyHash (getPubKeyHash), ScriptContext)
import PlutusTx qualified as Plutus

import Test.Vulcan.Unit.FinSet.MP (
  deinitCtx,
  deinitCtxBuilder,
  initCtx,
  initCtxBuilder,
  insertCtx,
  insertCtxBuilder,
  removeCtx,
  removeCtxBuilder,
  removedeinitCtx,
  removedeinitCtxBuilder,
 )
import Vulcan.Common.Types.FinSet (
  SepNodeAction (..),
 )
import Vulcan.Common.Types.Instances (Config (Config), TracingMode (..), Verbosity (..), tracingMode)
import Vulcan.Onchain.FinSet.MP.Separator (mkSepNodeMP)
import Vulcan.Utils.Scripts (wrapMintingPolicy')

import Plutarch.Context
import Test.Vulcan.CommonInputs

spec :: GoldenConf -> Spec
spec goldenConf = describe "FinSet Minting Policy with Separators" . pgoldenSpec' goldenConf $ do
  ---------------------------------------------
  -- Success
  ---------------------------------------------
  "Correct" @\ do
    -- Success Case :
    "Succesful SepInit transaction"
      @| let redm = SepInit
             ctx = initCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful DeInit transaction"
      @| let redm = SepDeinit
             ctx = deinitCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful Insert Seps transaction"
      @| let redm = InsertSeps [sep1, sep2, sep3, sep4] emptyNode
             ctx = insertSepCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful Remove Seps transaction"
      @| let redm = RemoveSeps [sep1, sep2, sep3, sep4] emptyNode
             ctx = removeSepCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful SepInsert transaction"
      @| let redm = SepInsert pk firstNode
             ctx = insertCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful SepRemove transaction"
      @| let redm = SepRemove pk firstNode
             ctx = removeCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful SepRemoveAndDeinit transaction"
      @| let redm = SepRemoveAndDeinit pk1
             ctx = removedeinitCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

  ----------------------------------------------------
  -- Failure:
  ----------------------------------------------------
  "Incorrect" @\ do
    -- Failure Case :
    "Failure: Insert Sep tries to insert a pkh"
      @| let redm = InsertSeps [getPubKeyHash pk] firstNode
             ctx = insertCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Insert Sep inserts in wrong order"
      @| let redm = InsertSeps [sep1, sep2, sep3, sep4] emptyNode
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ insertSepCtxBuilder
                    , output $
                        script finSetValHash
                          <> withValue (minAdaVal <> sepToken sep3 1)
                          <> withInlineDatum (mkD sepNode4)
                    , output $
                        script finSetValHash
                          <> withValue (minAdaVal <> sepToken sep4 1)
                          <> withInlineDatum (mkD incorrectSep)
                    , mint $ sepToken sep1 1 <> sepToken sep2 1 <> sepToken sep3 1 <> sepToken sep4 1
                    , signedWith pk
                    , timeRange $ to startTime
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Insert Sep incorrect mint"
      @| let redm = InsertSeps [sep1, sep2, sep3, sep4] emptyNode
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ insertSepCtxBuilder
                    , output $
                        script finSetValHash
                          <> withValue (minAdaVal <> sepToken sep3 1)
                          <> withInlineDatum (mkD sepNode4)
                    , output $
                        script finSetValHash
                          <> withValue (minAdaVal <> sepToken sep4 1)
                          <> withInlineDatum (mkD sepNode5)
                    , mint $ sepToken sep1 1 <> sepToken sep2 1 <> sepToken sep2 1 <> sepToken sep4 1
                    , signedWith pk
                    , timeRange $ to startTime
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Insert Sep not signed by seller"
      @| let redm = InsertSeps [sep1, sep2, sep3, sep4] emptyNode
             ctx =
              buildMinting checkVulcan $
                mkNormalized $
                  mconcat
                    [ insertSepCtxBuilder
                    , output $
                        script finSetValHash
                          <> withValue (minAdaVal <> sepToken sep3 1)
                          <> withInlineDatum (mkD sepNode4)
                    , output $
                        script finSetValHash
                          <> withValue (minAdaVal <> sepToken sep4 1)
                          <> withInlineDatum (mkD sepNode5)
                    , mint $ sepToken sep1 1 <> sepToken sep2 1 <> sepToken sep3 1 <> sepToken sep4 1
                    , timeRange $ to startTime
                    ]
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: Remove Seps tries to remove a pkh"
      @| let redm = RemoveSeps [getPubKeyHash pk] firstNode
             ctx = removeCtx
          in target # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

----------------------------------
-- Target Validator:

target :: ClosedTerm PMintingPolicy
target =
  wrapMintingPolicy' $
    mkSepNodeMP Config {tracingMode = DoTracing Verbose} marketCS finSetValHash
      # pconstant correctRef
      # pconstant separators

----------------------------------
-- Correct Contexts

insertSepCtxBuilder :: MintingBuilder
insertSepCtxBuilder =
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
          <> withRef ref1
    , input $
        pubKey pk
          <> withValue (doubleMinAdaVal <> doubleMinAdaVal)
    , output $
        script finSetValHash
          <> withValue (minAdaVal <> nodeToken Nothing 1)
          <> withInlineDatum (mkD sepNode1)
    , output $
        script finSetValHash
          <> withValue (minAdaVal <> sepToken sep1 1)
          <> withInlineDatum (mkD sepNode2)
    , output $
        script finSetValHash
          <> withValue (minAdaVal <> sepToken sep2 1)
          <> withInlineDatum (mkD sepNode3)
    ]

insertSepCtx :: ScriptContext
insertSepCtx =
  let builder :: MintingBuilder
      builder =
        mconcat
          [ insertSepCtxBuilder
          , output $
              script finSetValHash
                <> withValue (minAdaVal <> sepToken sep3 1)
                <> withInlineDatum (mkD sepNode4)
          , output $
              script finSetValHash
                <> withValue (minAdaVal <> sepToken sep4 1)
                <> withInlineDatum (mkD sepNode5)
          , mint $ sepToken sep1 1 <> sepToken sep2 1 <> sepToken sep3 1 <> sepToken sep4 1
          , signedWith pk
          , timeRange $ to startTime
          ]
   in buildMinting checkVulcan builder

removeSepCtxBuilder :: MintingBuilder
removeSepCtxBuilder =
  mconcat
    [ withMinting finSetCS
    , referenceInput $
        script otherValHash
          <> withValue (minAdaVal <> marketToken)
          <> withInlineDatum (mkD exampleMarket)
    , output $
        script finSetValHash
          <> withValue (minAdaVal <> nodeToken Nothing 1)
          <> withInlineDatum (mkD emptyNode)
    , input $
        script finSetValHash
          <> withValue (minAdaVal <> nodeToken Nothing 1)
          <> withInlineDatum (mkD sepNode1)
    , input $
        script finSetValHash
          <> withValue (minAdaVal <> sepToken sep1 1)
          <> withInlineDatum (mkD sepNode2)
    , input $
        script finSetValHash
          <> withValue (minAdaVal <> sepToken sep2 1)
          <> withInlineDatum (mkD sepNode3)
    , timeRange $ from closeTime
    ]

removeSepCtx :: ScriptContext
removeSepCtx =
  let builder :: MintingBuilder
      builder =
        mconcat
          [ removeSepCtxBuilder
          , input $
              script finSetValHash
                <> withValue (minAdaVal <> sepToken sep3 1)
                <> withInlineDatum (mkD sepNode4)
          , input $
              script finSetValHash
                <> withValue (minAdaVal <> sepToken sep4 1)
                <> withInlineDatum (mkD sepNode5)
          , mint $ sepToken sep1 (-1) <> sepToken sep2 (-1) <> sepToken sep3 (-1) <> sepToken sep4 (-1)
          , output $
              pubKey pk
                <> withValue (doubleMinAdaVal <> doubleMinAdaVal)
          , timeRange $ from closeTime
          ]
   in buildMinting checkVulcan (mkNormalized builder)
