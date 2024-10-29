module Test.Vulcan.Unit.FinSet.Validator (spec) where

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

import PlutusLedgerApi.V2 (
  Data (Constr),
  ScriptContext,
  always,
 )

import Vulcan.Common.Types.FinSet (setNodePrefix)
import Vulcan.Common.Types.Instances (Config (Config), TracingMode (..), Verbosity (..), tracingMode)
import Vulcan.Onchain.FinSet.Validator (pFinSetValidator)

import Plutarch.Context (
  SpendingBuilder,
  buildSpending,
  input,
  mint,
  mkNormalized,
  output,
  pubKey,
  script,
  timeRange,
  withInlineDatum,
  withRef,
  withSpendingOutRef,
  withValue,
 )
import Test.Vulcan.CommonInputs (
  checkVulcan,
  emptyNode,
  finSetValHash,
  minAdaVal,
  mkD,
  nodeToken,
  pk,
  privBidToken,
  ref1,
  ref2,
 )

spec :: GoldenConf -> Spec
spec goldenConf = describe "Node Validator" . pgoldenSpec' goldenConf $ do
  ---------------------------------------------
  -- Success
  ---------------------------------------------
  "Correct" @\ do
    "Succesful add node"
      @| let ctx = addToken
          in target # dat # redm # pconstant ctx @-> psucceeds

    "Succesful remove node"
      @| let ctx = removeToken
          in target # dat # redm # pconstant ctx @-> psucceeds

    "Succesful: add node with different node Token"
      @| let ctx =
              buildSpending checkVulcan $
                mkNormalized $
                  mconcat
                    [ addTokenBuilder
                    , input $
                        script finSetValHash
                          <> withValue (nodeToken (Just pk) 1)
                          <> withInlineDatum (mkD emptyNode)
                          <> withRef ref2
                    , mint $ nodeToken (Just pk) 1
                    ]
          in target # dat # redm # pconstant ctx @-> psucceeds
  ---------------------------------------------
  -- Failure
  ---------------------------------------------
  "Incorrect" @\ do
    "Failure: Does not mint NodeToken"
      @| let ctx =
              buildSpending checkVulcan $
                mkNormalized $
                  mconcat
                    [ addTokenBuilder
                    , input $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken Nothing 1)
                          <> withInlineDatum (mkD emptyNode)
                          <> withRef ref2
                    ]
          in target # dat # redm # pconstant ctx @-> pfails

    "Failure: Does not mint NodeToken"
      @| let ctx =
              buildSpending checkVulcan $
                mkNormalized $
                  mconcat
                    [ addTokenBuilder
                    , input $
                        script finSetValHash
                          <> withValue (minAdaVal <> nodeToken Nothing 1)
                          <> withInlineDatum (mkD emptyNode)
                          <> withRef ref2
                    , mint $ privBidToken 1
                    ]
          in target # dat # redm # pconstant ctx @-> pfails

----------------------------------
-- Target Validator

dat :: Term s PData
dat = pconstant $ Constr 0 []

redm :: Term s PData
redm = pconstant $ Constr 0 []

target :: ClosedTerm PValidator
target = pFinSetValidator Config {tracingMode = DoTracing Verbose} setNodePrefix

----------------------------------
-- Correct Contexts

addToken :: ScriptContext
addToken =
  let builder :: SpendingBuilder
      builder =
        mconcat
          [ addTokenBuilder
          , input $
              script finSetValHash
                <> withValue (minAdaVal <> nodeToken Nothing 1)
                <> withInlineDatum (mkD emptyNode)
                <> withRef ref2
          , input $ pubKey "deadbeefdeadbeefdeadbeef" <> withValue minAdaVal <> withRef ref1
          , mint $ nodeToken Nothing 1
          ]
   in buildSpending checkVulcan (mkNormalized builder)

addTokenBuilder :: SpendingBuilder
addTokenBuilder =
  mconcat
    [ output $
        script finSetValHash
          <> withValue (minAdaVal <> nodeToken Nothing 1)
          <> withInlineDatum (mkD emptyNode)
    , output $
        script finSetValHash
          <> withValue (minAdaVal <> nodeToken Nothing 1)
          <> withInlineDatum (mkD emptyNode)
    , timeRange always
    , withSpendingOutRef ref2
    ]

removeToken :: ScriptContext
removeToken =
  let builder :: SpendingBuilder
      builder =
        mconcat
          [ input $
              script finSetValHash
                <> withValue (minAdaVal <> nodeToken Nothing 1)
                <> withInlineDatum (mkD emptyNode)
                <> withRef ref2
          , output $ pubKey "deadbeefdeadbeefdeadbeef" <> withValue minAdaVal
          , mint $ nodeToken Nothing (-1)
          , timeRange always
          , withSpendingOutRef ref2
          ]
   in buildSpending checkVulcan (mkNormalized builder)
