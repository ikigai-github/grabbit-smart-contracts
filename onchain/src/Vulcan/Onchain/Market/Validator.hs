module Vulcan.Onchain.Market.Validator (pMarketValidator, mkMarketValidator) where

import Plutarch.Api.V2 (
  PScriptContext,
  PValidator,
 )
import PlutusLedgerApi.V2 (CurrencySymbol)

import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Types.Market (PMarketRedeemer)
import Vulcan.Utils (pverifyDataC)

-- | Does not allow to update MarketTerms dynamically.
mkMarketValidator ::
  Vulcan.Config ->
  CurrencySymbol ->
  ClosedTerm
    ( PData
        :--> PMarketRedeemer
        :--> PScriptContext
        :--> PUnit
    )
mkMarketValidator _cfg _marketCS = plam $ \_ _redm _ctx' -> perror

pMarketValidator :: Vulcan.Config -> CurrencySymbol -> ClosedTerm PValidator
pMarketValidator cfg cs = plam $ \datm redm' ctx -> unTermCont $ do
  redm <- pverifyDataC @PMarketRedeemer redm'
  pure . popaque $
    mkMarketValidator cfg cs
      # datm
      # pfromData redm
      # ctx
