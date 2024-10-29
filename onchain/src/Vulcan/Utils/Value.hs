{-# LANGUAGE AllowAmbiguousTypes #-}

module Vulcan.Utils.Value (
  pposSingleton,
  plovelace,
  pisPrefixOf,
  phasCS,
  pgetCS,
  phasAsset,
  pcontainsValue,
  pfindCurrencySymbolsByTokenPrefix,
  pcontainsCurrencySymbols,
  padaTolovelace,
  adaTolovelace,
  psumData,
  (#-),
  ppositiveSingleton,
  ponlySingletonValue,
  pamountOfAssets,
) where

import Data.Data (Proxy (Proxy))
import GHC.TypeNats (KnownNat, Nat, natVal, type (<=))
import Plutarch.Api.V1 (
  AmountGuarantees (NonZero, Positive),
  KeyGuarantees (Sorted),
  PCurrencySymbol,
  PMap (PMap),
  PTokenName,
  PValue (PValue),
 )
import Plutarch.Api.V1.AssocMap qualified as Map
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Num (PNum)
import Plutarch.Positive (PPositive)
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Utils.List (ponlyElem)

-- | Converts an Ada amount to Lovelace
adaTolovelace :: forall (n :: Type). Num n => n -> n
adaTolovelace = (*) 1_000_000

-- | Converts an Ada amount to Lovelace at a term level
padaTolovelace :: PNum n => ClosedTerm (n :--> n)
padaTolovelace = phoistAcyclic $ plam adaTolovelace

-- | Subtracts one Value from another
(#-) ::
  forall (s :: S).
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PValue 'Sorted 'NonZero)
a #- b = Value.pnormalize #$ Value.punionWith # plam (-) # a # b

pposSingleton ::
  forall (n :: Nat) (s :: S).
  (KnownNat n, 1 <= n) =>
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s (PValue 'Sorted 'Positive)
pposSingleton cs tn =
  punsafeCoerce $
    Value.psingleton # cs # tn # pconstant (toInteger amount)
  where
    amount = natVal @n Proxy

-- | Parses the only asset in singleton value, fails otherwise.
ponlySingletonValue ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees).
  Vulcan.Config ->
  ClosedTerm
    ( PValue anyOrder anyAmount
        :--> PPair
              (PAsData PCurrencySymbol)
              ( PBuiltinPair
                  (PAsData PTokenName)
                  (PAsData PInteger)
              )
    )
ponlySingletonValue cfg = phoistAcyclic $
  plam $ \value -> P.do
    PValue csMap <- pmatch value
    let pair = ponlyAssoc cfg # csMap
        cs = pfstBuiltin # pair
        token = ponlyAssoc cfg #$ pfromData $ psndBuiltin # pair
    pcon $ PPair cs token

-- | Checks if Value contains non-zero amouny of a particular asset
phasAsset ::
  ClosedTerm
    ( PValue 'Sorted 'Positive
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PBool
    )
phasAsset = phoistAcyclic $
  plam $ \value cs tn ->
    pnot #$ 0 #== Value.pvalueOf # value # cs # tn

-- | Calculates the total number of assets within a Value.
pamountOfAssets ::
  ClosedTerm
    (PValue 'Sorted 'Positive :--> PInteger)
pamountOfAssets = phoistAcyclic $
  plam $
    flip pmatch $ \(PValue val) -> P.do
      PMap csPairs <- pmatch val
      let tokensLength pair = P.do
            PMap tokens <- pmatch $ pfromData $ psndBuiltin # pair
            plength # tokens
      pfoldl # plam (\acc -> (acc +) . tokensLength) # 0 # csPairs

-- | Checks if the second Value is completely contained within the first
pcontainsValue ::
  ClosedTerm
    ( PValue 'Sorted 'Positive
        :--> PValue 'Sorted 'Positive
        :--> PBool
    )
pcontainsValue = phoistAcyclic $
  plam $ \superset subset -> P.do
    let forEachCS = plam $ \csPair ->
          let cs = pfromData $ pfstBuiltin # csPair
              tnMap = pto $ pfromData $ psndBuiltin # csPair
           in pall # forEachTN cs # tnMap

        forEachTN cs = plam $ \tnPair ->
          let tn = pfromData $ pfstBuiltin # tnPair
              amount = pfromData $ psndBuiltin # tnPair
           in amount #<= Value.pvalueOf # superset # cs # tn

    pall # forEachCS #$ pto $ pto subset

-- | Finds the first CurrencySymbol associated with a TokenName
pgetCS ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees).
  ClosedTerm
    ( PValue anyOrder anyAmount
        :--> PTokenName
        :--> PMaybe PCurrencySymbol
    )
pgetCS = phoistAcyclic $
  plam $ \v expectedTN ->
    let mapVal = pto v
        hasTN = pfilterMap # (panyKey # plam (expectedTN #==)) # mapVal
     in pmatch (pto hasTN) $ \case
          PNil -> pcon PNothing
          PCons head _ -> pcon $ PJust $ pfromData $ pfstBuiltin # head

-- | Turns an integer into a value in lovelace. Integer must be positive
plovelace :: ClosedTerm (PPositive :--> PValue 'Sorted 'Positive)
plovelace = phoistAcyclic $
  plam $ \amount ->
    ppositiveSingleton # Value.padaSymbol # Value.padaToken # amount

{- | Construct a singleton 'PValue' containing only the given quantity of the given currency.
  Errors if quantity is not Positive.
-}
ppositiveSingleton ::
  ClosedTerm
    ( PCurrencySymbol
        :--> PTokenName
        :--> PPositive
        :--> PValue 'Sorted 'Positive
    )
ppositiveSingleton = phoistAcyclic $
  plam $ \symbol token amount ->
    punsafeDowncast $
      Map.psingleton # symbol #$ Map.psingleton # token # pto amount

{- | Finds the associated Currency symbols that contain token
  names prefixed with the ByteString.
-}
pfindCurrencySymbolsByTokenPrefix ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees).
  ClosedTerm
    ( PValue anyOrder anyAmount
        :--> PByteString
        :--> PBuiltinList (PAsData PCurrencySymbol)
    )
pfindCurrencySymbolsByTokenPrefix = phoistAcyclic $
  plam $ \value prefix ->
    let mapVal = pto value
        isPrefixed =
          pfilterMap
            # (panyKey # plam (\key -> pisPrefixedWith # key # prefix))
            # mapVal
     in pmap # pfstBuiltin #$ pto isPrefixed

-- | Checks that a Value contains all the given CurrencySymbols.
pcontainsCurrencySymbols ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees).
  ClosedTerm
    ( PValue anyOrder anyAmount
        :--> PBuiltinList (PAsData PCurrencySymbol)
        :--> PBool
    )
pcontainsCurrencySymbols = phoistAcyclic $
  plam $ \inValue symbols ->
    let value = pmap # pfstBuiltin #$ pto $ pto inValue
        containsCS = plam $ \cs -> pelem # cs # value
     in pall # containsCS # symbols

-- | Checks if a Currency Symbol is held within a Value
phasCS ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees).
  ClosedTerm
    (PValue anyOrder anyAmount :--> PCurrencySymbol :--> PBool)
phasCS = phoistAcyclic $
  plam $ \value symbol ->
    panyKey # plam (#== symbol) # pto value

-----------------------------
-- helpers

-- | Tests if any key in the map satisfies the given predicate.
panyKey ::
  forall (kg :: KeyGuarantees) (k :: PType) (v :: PType).
  PIsData k =>
  ClosedTerm ((k :--> PBool) :--> PMap kg k v :--> PBool)
panyKey = phoistAcyclic $
  plam $ \p map ->
    pany # plam (\pair -> p #$ pfromData $ pfstBuiltin # pair) # pto map

-- | Filters the map so it contains only the values that satisfy the given predicate.
pfilterMap ::
  forall (kg :: KeyGuarantees) (k :: PType) (v :: PType).
  PIsData v =>
  ClosedTerm ((v :--> PBool) :--> PMap kg k v :--> PMap kg k v)
pfilterMap = phoistAcyclic $
  plam $ \p ->
    Map.pmapMaybe #$ plam $ \v -> pif (p # v) (pcon $ PJust v) (pcon PNothing)

-- | Sum a PBuiltinlist of PAsData Integers
psumData :: ClosedTerm (PBuiltinList (PAsData PInteger) :--> PInteger)
psumData = pfoldr # plam ((+) . pfromData) # 0

-- | Checks if a tokenName is prefixed by a certain ByteString
pisPrefixedWith :: ClosedTerm (PTokenName :--> PByteString :--> PBool)
pisPrefixedWith = plam $ \tn prefix -> P.do
  let tnBS = pto tn
  pisPrefixOf # prefix # tnBS

-- | Checks if the first ByteString is a prefix of the second
pisPrefixOf :: ClosedTerm (PByteString :--> PByteString :--> PBool)
pisPrefixOf = plam $ \prefix src -> P.do
  let prefixLength = plengthBS # prefix
      prefix' = psliceBS # 0 # prefixLength # src
  prefix' #== prefix

-- | Gets the only key-value pair from singleton AssocMap and fails otherwise.
ponlyAssoc ::
  forall (kg :: KeyGuarantees) (k :: PType) (v :: PType).
  Vulcan.Config ->
  ClosedTerm (PMap kg k v :--> PBuiltinPair (PAsData k) (PAsData v))
ponlyAssoc cfg = phoistAcyclic $
  plam $ \map -> P.do
    PMap m <- pmatch map
    ponlyElem cfg # m
