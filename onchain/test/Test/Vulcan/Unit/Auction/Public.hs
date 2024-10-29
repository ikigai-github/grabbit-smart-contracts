module Test.Vulcan.Unit.Auction.Public () where

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

import PlutusLedgerApi.V1.Interval (interval, to)
import PlutusTx qualified as Plutus

import Vulcan.Onchain.Auction.Validator.Public (mkPublicAuctionValidator)
import Vulcan.Utils.Scripts (wrapValidator')

import Vulcan.Common.Types.Auction

import Plutarch.Context
import PlutusLedgerApi.V2 (ScriptContext)
import Test.Vulcan.CommonInputs

-- Needs Updating
{-

spec :: GoldenConf -> Spec
spec goldenConf = describe "Public Auction Validator" . pgoldenSpec' goldenConf $ do
  ---------------------------------------------
  -- Success
  ---------------------------------------------
  "Correct" @\ do
    -- Success Case :
    "Succesful EnrollAct Tranasction"
      @| let redm = PubEnrollAct stateCS
             ctx = buildCtxSpending enrollCtx
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

    "Succesful EnrollAct Tranasction with Bid"
      @| let redm = PubEnrollAct stateCS
             ctx = buildCtxSpending bidEnrollCtx
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> psucceeds

  ----------------------------------------------------
  -- Failure:
  ----------------------------------------------------
  "Incorrect" @\ do
    -- Failure Case :
    "Failure: EnrollAct: Bid Placed before Start"
      @| let redm = PubEnrollAct stateCS
             ctx =
              buildCtxSpending $
                bidEnrollCtx
                  { time = to 800_000
                  }
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

    "Failure: EnrollAct: Not signed by bidder"
      @| let redm = PubEnrollAct stateCS
             ctx =
              buildCtxSpending $
                enrollCtx
                  { sigs = [pk2]
                  }
          in target # dat # pconstant (Plutus.toData redm) # pconstant ctx @-> pfails

----------------------------------
-- Target Validator

dat :: Term s PData
dat = pconstant $ Plutus.toData $ MkAuctionDatum exampleMarket

target :: ClosedTerm PValidator
target = wrapValidator' mkPublicAuctionValidator

----------------------------------
-- Correct Contexts
-}
