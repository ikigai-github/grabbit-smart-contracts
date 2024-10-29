module Vulcan.Utils.List (
  ptryFind,
  pfind,
  ppartitionWith,
  pconvertWithRest,
  pfindWithRest,
  pfirstJust,
  pmapMaybe,
  ptryFirstJust,
  ppartition,
  ponlyElem,
) where

import Plutarch.Monadic qualified as P
import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Utils.Prelude (passert, traceError)

{- | Partition input list on two, where first consists of
  elements that satisfies predicate and second are not.
-}
ppartition ::
  forall (list :: PType -> PType) (a :: PType).
  PListLike list =>
  PElemConstraint list a =>
  ClosedTerm
    ( (a :--> PBool)
        :--> list a
        :--> PPair (list a) (list a)
    )
ppartition = phoistAcyclic $
  plam $ \f xs ->
    let distinct = plam $ \pacc pa -> P.do
          PPair bs as <- pmatch pacc
          pmatch (f # pa) $ \case
            PTrue -> pcon $ PPair (pcons # pa # bs) as
            PFalse -> pcon $ PPair bs (pcons # pa # as)
     in pfoldl # distinct # pcon (PPair pnil pnil) # xs

{- | Tries to parse all elements, returns the list of successfully parsed elements,
  and a list of elements that failed to parse.
-}
ppartitionWith ::
  forall (list :: PType -> PType) (a :: PType) (b :: PType).
  PListLike list =>
  PElemConstraint list a =>
  PElemConstraint list b =>
  ClosedTerm
    ( (a :--> PMaybe b)
        :--> list a
        :--> PPair (list b) (list a)
    )
ppartitionWith = phoistAcyclic $
  plam $ \f xs ->
    let distinct = plam $ \pacc pa -> P.do
          PPair bs as <- pmatch pacc
          pmatch (f # pa) $ \case
            PNothing -> pcon $ PPair bs (pcons # pa # as)
            PJust pb -> pcon $ PPair (pcons # pb # bs) as
     in pfoldl # distinct # pcon (PPair pnil pnil) # xs

{- | Returns first successfully parsed element and a input list without that element.
  Fails if no element successfully parsed.
-}
pconvertWithRest ::
  forall (list :: PType -> PType) (a :: PType) (b :: PType).
  PListLike list =>
  PElemConstraint list a =>
  ClosedTerm
    ( (a :--> PMaybe b)
        :--> list a
        :--> PPair b (list a)
    )
pconvertWithRest = phoistAcyclic $
  plam $ \f ys ->
    let mcons self x xs =
          pmatch (f # x) $ \case
            PJust b -> P.do
              acc <- plam
              pcon $ PPair b (pconcat # acc # xs)
            PNothing -> P.do
              acc <- plam
              self # xs #$ pcons # x # acc
        mnil = const (ptraceError "Conv")
     in precList mcons mnil # ys # pnil

{- | Finds the element that satisfies predicate,
  also returns input list without found element.

  Fails if nothing satisfies predicate.
-}
pfindWithRest ::
  forall (list :: PType -> PType) (a :: PType).
  PListLike list =>
  PElemConstraint list a =>
  ClosedTerm
    ( (a :--> PBool)
        :--> list a
        :--> PPair a (list a)
    )
pfindWithRest = phoistAcyclic $
  plam $ \f ys ->
    let mcons self x xs =
          pmatch (f # x) $ \case
            PTrue -> P.do
              acc <- plam
              pcon $ PPair x (pconcat # acc # xs)
            PFalse -> P.do
              acc <- plam
              self # xs #$ pcons # x # acc
        mnil = const (ptraceError "Find")
     in precList mcons mnil # ys # pnil

{- | Finds the first element of the list that
 satisfies the predicate and fails if none do.
-}
ptryFind :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> a)
ptryFind = phoistAcyclic $
  plam $ \predicate ->
    precList
      ( \self x xs ->
          pif
            (predicate # x)
            x
            (self # xs)
      )
      (const $ ptraceError "No element")

-- | Finds first successfult parse result, fails if none.
ptryFirstJust ::
  forall (list :: PType -> PType) (a :: PType) (b :: PType).
  PListLike list =>
  PElemConstraint list a =>
  ClosedTerm ((a :--> PMaybe b) :--> list a :--> b)
ptryFirstJust = phoistAcyclic $
  plam $ \predicate ->
    precList
      ( \self x xs ->
          pmatch
            (predicate # x)
            $ \case
              PJust y -> y
              PNothing -> (self # xs)
      )
      (const $ ptraceError "ptryFirstJust")

-- | Finds first successfult parse result.
pfirstJust ::
  forall (list :: PType -> PType) (a :: PType) (b :: PType).
  PListLike list =>
  PElemConstraint list a =>
  ClosedTerm ((a :--> PMaybe b) :--> list a :--> PMaybe b)
pfirstJust = phoistAcyclic $
  plam $ \predicate ->
    precList
      ( \self x xs ->
          pmatch
            (predicate # x)
            $ \case
              PJust y -> pcon $ PJust y
              PNothing -> (self # xs)
      )
      (const $ pcon PNothing)

-- | Returns the only singleton list elem, fails otherwise.
ponlyElem ::
  forall (list :: PType -> PType) (a :: PType).
  (PListLike list, PElemConstraint list a) =>
  Vulcan.Config ->
  ClosedTerm (list a :--> a)
ponlyElem cfg = phoistAcyclic $
  plam $ \xs -> P.do
    let match y ys = P.do
          passert cfg "List contains more than one element." $ pnull # ys
          y
    pelimList match (traceError cfg "Empty list") xs

-- | Is a version of @map@ which can throw out elements.
pmapMaybe ::
  forall (list :: PType -> PType) (a :: PType) (b :: PType).
  PListLike list =>
  PElemConstraint list a =>
  PElemConstraint list b =>
  ClosedTerm ((a :--> PMaybe b) :--> list a :--> list b)
pmapMaybe =
  phoistAcyclic $
    plam $ \func ->
      precList
        ( \self x xs ->
            pmatch (func # x) $ \case
              PJust y -> (pcons # y # (self # xs))
              PNothing -> (self # xs)
        )
        (const pnil)
