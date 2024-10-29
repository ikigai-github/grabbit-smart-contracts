{-# LANGUAGE AllowAmbiguousTypes #-}

module Vulcan.Utils.Patterns (pfieldh, pfield0, pfield1) where

import GHC.TypeLits (KnownNat, Nat, Symbol)

import Plutarch.DataRepr.Internal (PLabelIndex, PUnLabel)
import Plutarch.DataRepr.Internal.Field (PDataFields (PFields))
import Plutarch.DataRepr.Internal.FromData (PFromDataable)
import Plutarch.DataRepr.Internal.HList (IndexList)

-- | Haskell level 'pfield'. Intended to be used in ViewPatterns.
pfieldh ::
  forall
    (name :: Symbol)
    (p :: PType)
    (s :: S)
    (a :: PType)
    (as :: [PLabeledType])
    (n :: Nat)
    (b :: PType).
  ( PDataFields p
  , as ~ PFields p
  , n ~ PLabelIndex name as
  , KnownNat n
  , a ~ PUnLabel (IndexList n as)
  , PFromDataable a b
  ) =>
  Term s p ->
  Term s b
pfieldh x = pfield @name # x

-- | Haskell level pfield with the label set to "_0". Intended to be used in ViewPatterns.
pfield0 ::
  forall
    (p :: PType)
    (s :: S)
    (a :: PType)
    (as :: [PLabeledType])
    (n :: Nat)
    (b :: PType).
  ( PDataFields p
  , as ~ PFields p
  , n ~ PLabelIndex "_0" as
  , KnownNat n
  , a ~ PUnLabel (IndexList n as)
  , PFromDataable a b
  ) =>
  Term s p ->
  Term s b
pfield0 = pfieldh @"_0"

-- | Haskell level pfield with the label set to "_1". Intended to be used in ViewPatterns.
pfield1 ::
  forall
    (p :: PType)
    (s :: S)
    (a :: PType)
    (as :: [PLabeledType])
    (n :: Nat)
    (b :: PType).
  ( PDataFields p
  , as ~ PFields p
  , n ~ PLabelIndex "_1" as
  , KnownNat n
  , a ~ PUnLabel (IndexList n as)
  , PFromDataable a b
  ) =>
  Term s p ->
  Term s b
pfield1 = pfieldh @"_1"
