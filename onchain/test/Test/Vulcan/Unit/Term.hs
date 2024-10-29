module Test.Vulcan.Unit.Term (spec) where

import Control.Arrow (Arrow ((***)))
import Control.Monad (forM_)
import Data.List (nub, permutations)
import Data.String (IsString (fromString))
import Ledger.Exports.V2 (Address, OutputDatum (NoOutputDatum), TxOut (TxOut, txOutAddress, txOutDatum, txOutReferenceScript, txOutValue))
import Plutarch.Api.V1.AssocMap qualified as Map
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Positive (ptryPositive)
import Plutarch.Test (
  GoldenConf,
  passert,
  passertNot,
  pfails,
  pgoldenSpec',
  psucceeds,
  (@->),
  (@\),
  (@|),
 )
import PlutusLedgerApi.V1.Value (Value)
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as AssocMap
import Test.Hspec (Spec, describe)
import Test.Vulcan.CommonInputs
import Vulcan.Onchain.Auction.Validator.Helpers (paymentToBeneficiaries)

spec :: GoldenConf -> Spec
spec goldenConf = describe "Term tests" . pgoldenSpec' goldenConf $ do
  let total = ada 100
  ---------------------------------------------
  -- Success
  ---------------------------------------------
  "Correct" @\ do
    "Successful paymentToBeneficiaries" @\ do
      "Correct when no beneficiaries"
        @| bensPaid
          AssocMap.empty
          total
          [ out (fst beneficiary1, 500)
          , out (fst beneficiary3, 280)
          ]
        @-> passert

      "With three beneficiaries" @\ do
        let distributions = mkBensDistribution <$> correctFundsDistributions
        forM_ distributions $ \bensDistribution -> do
          let outs = out <$> bensDistribution
              parts = fmap snd bensDistribution :: [Integer]
          "Case: " <> fromString (show parts)
            @| bensPaid (AssocMap.fromList bensDistribution) total outs @-> passert

  "Incorrect" @\ do
    -- here you can reproduce some exact case
    "Custom case"
      @| let goodDistrib = mkBensDistribution [10, 70, 20]
             wrongDistrib = mkBensDistribution [10, 69, 20]
             outs = out <$> wrongDistrib
          in bensPaid (AssocMap.fromList goodDistrib) total outs @-> passertNot

    -- TODO: investigate why this succeeds. Likely a bug?
    "Bug case"
      @| bensPaid
        ( AssocMap.fromList
            [ (fst beneficiary1, 10)
            , (fst beneficiary2, 70)
            , (fst beneficiary3, 20)
            ]
        )
        total
        [ out (fst beneficiary1, 10)
        , out (fst beneficiary2, 50)
        , out (fst beneficiary3, 20)
        ]
        @-> passertNot

    "Fails on wrong distribution" @\ do
      let distributions =
            (zip addresses *** zip addresses)
              <$>
              -- One of beneficiaries gets 1 less ADA in every distribution
              distortFundsDistributions
                (-)
                correctFundsDistributions
                (nub $ permutations [0, 0, 1])
      forM_ distributions $ \(distrib, corruptedDistrib) -> do
        let outs = out <$> corruptedDistrib -- outputs are corrupted
            corruptedParts = fromString $ show $ snd <$> corruptedDistrib
            parts = fromString $ show $ snd <$> distrib
        "Case: declared " <> parts <> ", outputs " <> corruptedParts
          @| bensPaid (AssocMap.fromList distrib) total outs @-> passertNot

----------------------------------
-- Constants and tools

-- | `paymentToBeneficiaries` unlifted except return value
bensPaid :: Map Address Integer -> Value -> [TxOut] -> ClosedTerm PBool
bensPaid bens bid outs =
  paymentToBeneficiaries
    # (Map.pmap # ptryPositive #$ Map.passertSorted # pconstant bens)
    # (Value.passertPositive #$ Value.passertSorted # pconstant bid)
    # pconstant outs

addresses :: [Address]
addresses = fst <$> [beneficiary1, beneficiary2, beneficiary3]

-- | Must be 100 summary in each row
correctFundsDistributions :: forall (n :: Type). (Num n, Eq n) => [[n]]
correctFundsDistributions =
  nub $
    permutations
      =<< [ [10, 40, 50]
          , [20, 20, 60]
          , [10, 10, 80]
          , [70, 20, 10]
          ]

-- | The way to distort correct distribution
distortFundsDistributions ::
  forall (a :: Type) (b :: Type) (c :: Type).
  (a -> b -> c) ->
  [[a]] ->
  -- | Correct distribution
  [[b]] ->
  -- | Distortion
  [([a], [c])]
distortFundsDistributions op correctDistributions distortions =
  correctDistributions >>= \distrib ->
    (distrib,) . zipWith op distrib <$> distortions

out :: (Address, Integer) -> TxOut
out (addr, amount) =
  TxOut
    { txOutAddress = addr
    , txOutValue = ada amount
    , txOutDatum = NoOutputDatum
    , txOutReferenceScript = Nothing
    }

mkBensDistribution :: forall (b :: Type). [b] -> [(Address, b)]
mkBensDistribution = zip addresses
