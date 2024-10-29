module Vulcan.Utils.Prelude (
  pcheck,
  pbindMaybe,
  psatisfies,
  passert,
  passertManually,
  percentage,
  traceError,
  pabs,
  paltMaybe,
) where

import Data.Text qualified as T
import Plutarch.Rational (ptruncate)
import Vulcan.Common.Types.Instances qualified as Vulcan

-- | Plutarch-level @>>=@ for @Maybe@
pbindMaybe ::
  forall (s :: S) (a :: PType) (b :: PType).
  Term s (PMaybe a :--> (a :--> PMaybe b) :--> PMaybe b)
pbindMaybe = phoistAcyclic $
  plam $ \mx f -> P.do
    pmatch mx $ \case
      PNothing -> pcon PNothing
      PJust x -> f # x

-- | Literally @<|>@
paltMaybe ::
  forall (s :: S) (a :: PType).
  Term s (PMaybe a) ->
  Term s (PMaybe a) ->
  Term s (PMaybe a)
paltMaybe a b =
  pmatch a $ \case
    PNothing -> b
    x -> pcon x

{- | Tags value as succesful (@Just a@) or failed (@Nothing@)
 accordingly to a given @Bool@ flag.
-}
psatisfies ::
  forall (a :: PType).
  ClosedTerm ((a :--> PBool) :--> (a :--> PMaybe a))
psatisfies = phoistAcyclic $
  plam $ \p x ->
    pif (p # x) (pcon $ PJust x) (pcon PNothing)

{- | If the input is True then continue otherwise throw an error message.
  Choices error message by current tracing mode.
  Useful when error message calculating on-chain.
-}
passertManually ::
  forall (s :: S) (a :: PType).
  Vulcan.Config ->
  Term s PString -> -- short trace
  Term s PString -> -- long trace
  Term s PBool ->
  Term s a ->
  Term s a
passertManually cfg shortErrorMsg longErrorMsg b inp =
  pif b inp $
    ptraceError $
      if isConsciseTracing cfg
        then shortErrorMsg
        else longErrorMsg

isConsciseTracing :: Vulcan.Config -> Bool
isConsciseTracing ((.tracingMode) -> Vulcan.DoTracing Vulcan.Conscise) = True
isConsciseTracing _ = False

-- | Traces error accordingly to a given verbosity level
traceError :: forall (s :: S) (a :: PType). Vulcan.Config -> T.Text -> Term s a
traceError cfg =
  ptraceError . pconstant
    . if isConsciseTracing cfg then shortString else id

-- | Shortens text to keep only first letters of it's words
shortString :: T.Text -> T.Text
shortString =
  T.concat
    . fmap (T.singleton . T.head)
    . T.words

{- | If the input is True then continue otherwise throw an error message.
   Short trace is a sequence of first letters of long trace words.
-}
passert ::
  forall (s :: S) (a :: PType).
  Vulcan.Config ->
  T.Text -> -- long trace
  Term s PBool ->
  Term s a ->
  Term s a
passert cfg longErrorMsg b inp = pif b inp $ traceError cfg longErrorMsg

-- | If the input is True then returns PJust otherwise PNothing
pcheck :: forall (s :: S) (a :: PType). Term s PBool -> Term s a -> Term s (PMaybe a)
pcheck b inp = pif b (pcon $ PJust inp) (pcon PNothing)

-- | Gives the abosolute value of an Integer
pabs :: ClosedTerm (PInteger :--> PInteger)
pabs = phoistAcyclic $
  plam $ \int ->
    pmatch (int #< 0) $ \case
      PTrue -> (-1) * int
      PFalse -> int

-- | Calculates the percentage and truncates the result.
percentage :: ClosedTerm (PInteger :--> PInteger :--> PInteger)
percentage = phoistAcyclic $
  plam $ \percent total ->
    let rational = pcon $ PRational (percent * total) 100
     in pif (percent #<= 0 #|| total #<= 0) 0 (ptruncate # rational)
