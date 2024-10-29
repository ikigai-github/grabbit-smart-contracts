module Vulcan.Onchain.Market.MP (mkMarketMP, pMarketMP, marketMP) where

import Plutarch.Api.V1 (PCurrencySymbol, PTokenName)
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Api.V1.Value qualified as V
import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
  PScriptPurpose (PMinting),
  PTxOut,
  mkMintingPolicy,
 )
import Plutarch.Monadic qualified as P

import Data.Data (Proxy (Proxy))
import PlutusLedgerApi.V2 (MintingPolicy, TxOutRef)
import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.SpecialUTxO.Utils (parseMarketOutputUtxo)
import Vulcan.Types.Market (PMarketTerms)
import Vulcan.Utils (
  passert,
  pposSingleton,
  pverifyDataC,
  toPlutarchConfig,
 )
import Vulcan.Utils.Value (ppositiveSingleton)

mkMarketMP ::
  Vulcan.Config ->
  TxOutRef ->
  ClosedTerm (PMarketTerms :--> PScriptContext :--> PUnit)
mkMarketMP cfg oref = plam $ \marketTerms ctx' -> P.do
  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  info <- pletFields @'["inputs", "outputs", "mint"] ctx.txInfo
  inputs <- plet $ pfromData info.inputs
  outputs <- plet $ pfromData info.outputs
  ownCS <- plet $ P.do
    PMinting mintRecord <- pmatch $ ctx.purpose
    pfield @"_0" # mintRecord
  ownTN <- plet $ pconstant "MarketEscrow"

  -- Inputs:
  passert cfg "TxOutRef Must be consumed" $
    pany # plam (\input -> pconstantData oref #== (pfield @"outRef" # input)) # inputs
  -- Outputs:
  passert cfg "Must output datum" $
    pany
      # plam (pCheckMarketOut # ownCS # ownTN # marketTerms #)
      # outputs
  -- Mint:
  passert cfg "Must mint only 1 MarketEscrow token" $
    (pnormalize # pfromData info.mint) #== V.psingleton # ownCS # ownTN # 1

  pconstant ()

-- | Ensures MarketEscrow output is correct
pCheckMarketOut ::
  ClosedTerm
    ( PCurrencySymbol
        :--> PTokenName
        :--> PMarketTerms
        :--> PTxOut
        :--> PBool
    )
pCheckMarketOut = phoistAcyclic $
  plam $ \marketCS marketTN terms out -> P.do
    let marketToken = pposSingleton @1 marketCS marketTN
    PJust actualDatum <- pmatch $ parseMarketOutputUtxo # marketToken # out
    actualDatumF <- pletFields @'["fixedFee", "percentageFee"] actualDatum
    terms #== pfromData actualDatum
      #&& 0 #<= pfromData actualDatumF.fixedFee
      #&& 0 #<= pfromData actualDatumF.percentageFee
      #&& pfromData actualDatumF.percentageFee #<= 100

------------------------------------------
-- Scripts

pMarketMP ::
  Vulcan.Config ->
  TxOutRef ->
  Term s PMintingPolicy
pMarketMP cfg ref =
  plam $ \redm' ctx -> unTermCont $ do
    redm <- pverifyDataC @PMarketTerms redm'
    pure . popaque $
      mkMarketMP cfg ref
        # pfromData redm
        # ctx

-- | Compile the Market Minting Policy
marketMP :: Vulcan.Config -> TxOutRef -> MintingPolicy
marketMP cfg oref =
  mkMintingPolicy (toPlutarchConfig cfg) $ pMarketMP cfg oref
