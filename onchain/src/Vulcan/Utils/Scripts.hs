-- FIX ME remove when upstream
module Vulcan.Utils.Scripts (
  wrapMintingPolicy,
  wrapMintingPolicy',
  wrapValidator,
  wrapValidator',
  wrapMintingPolicyWith,
  wrapValidatorWith,
  pverifyDataC,
) where

import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
  PValidator,
  mkMintingPolicy,
  mkValidator,
 )
import Plutarch.Internal (Config)
import PlutusLedgerApi.V1.Scripts (MintingPolicy, Validator)

-- | Effortless 'mkMintingPolicy'. Handles data verification as well.
wrapMintingPolicy ::
  forall (redm :: PType).
  (PTryFrom PData (PAsData redm), PIsData redm) =>
  Config ->
  ClosedTerm (redm :--> (PScriptContext :--> PUnit)) ->
  MintingPolicy
wrapMintingPolicy config = wrapMintingPolicyWith (mkMintingPolicy config)

-- | Plutarch term returning version of 'wrapMintingPolicy'.
wrapMintingPolicy' ::
  forall (redm :: PType).
  (PTryFrom PData (PAsData redm), PIsData redm) =>
  ClosedTerm (redm :--> (PScriptContext :--> PUnit)) ->
  ClosedTerm PMintingPolicy
wrapMintingPolicy' pf = wrapMintingPolicyWith (\x -> x) pf
{-# ANN wrapMintingPolicy' ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN wrapMintingPolicy' ("HLint: ignore Use id" :: String) #-}

-- | Effortless 'mkValidator'. Handles data verification as well.
wrapValidator ::
  forall (datum :: PType) (redeemer :: PType).
  ( PTryFrom PData (PAsData datum)
  , PTryFrom PData (PAsData redeemer)
  , PIsData datum
  , PIsData redeemer
  ) =>
  Config ->
  ClosedTerm (datum :--> redeemer :--> PScriptContext :--> PUnit) ->
  Validator
wrapValidator config = wrapValidatorWith (mkValidator config)

-- | Plutarch term returning version of 'wrapValidator'.
wrapValidator' ::
  forall (datum :: PType) (redeemer :: PType).
  ( PTryFrom PData (PAsData datum)
  , PTryFrom PData (PAsData redeemer)
  , PIsData datum
  , PIsData redeemer
  ) =>
  ClosedTerm (datum :--> redeemer :--> PScriptContext :--> PUnit) ->
  ClosedTerm PValidator
wrapValidator' pf = wrapValidatorWith (\x -> x) pf
{-# ANN wrapValidator' ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN wrapValidator' ("HLint: ignore Use id" :: String) #-}

wrapMintingPolicyWith ::
  forall (redeemer :: PType) (a :: Type).
  (PTryFrom PData (PAsData redeemer), PIsData redeemer) =>
  (ClosedTerm PMintingPolicy -> a) ->
  ClosedTerm (redeemer :--> PScriptContext :--> PUnit) ->
  a
wrapMintingPolicyWith f pf = f $
  plam $ \rawRedeemer ctx -> unTermCont $ do
    redeemer <- pverifyDataC @redeemer rawRedeemer
    pure . popaque $ pf # pfromData redeemer # ctx

wrapValidatorWith ::
  forall (datum :: PType) (redeemer :: PType) (a :: Type).
  ( PTryFrom PData (PAsData datum)
  , PTryFrom PData (PAsData redeemer)
  , PIsData datum
  , PIsData redeemer
  ) =>
  (ClosedTerm PValidator -> a) ->
  ClosedTerm (datum :--> redeemer :--> PScriptContext :--> PUnit) ->
  a
wrapValidatorWith f pf = f $
  plam $ \rawDatum rawRedeemer ctx -> unTermCont $ do
    datum <- pverifyDataC @datum rawDatum
    redeemer <- pverifyDataC @redeemer rawRedeemer
    pure . popaque $ pf # pfromData datum # pfromData redeemer # ctx

-- | Verifies that data is well-formed
pverifyDataC ::
  forall (a :: PType) (r :: PType) (s :: S).
  PTryFrom PData (PAsData a) =>
  Term s PData ->
  TermCont @r s (Term s (PAsData a))
pverifyDataC = fmap fst . tcont . ptryFrom
