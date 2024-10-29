{-# OPTIONS_GHC -Wno-orphans #-}

module Onchain.Collections.MerkleTree where

import Control.Applicative ((<|>))
import Plutarch
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude hiding (PLeft, PRight)
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC
import PlutusLedgerApi.V2 (BuiltinByteString)
import PlutusTx qualified
import PlutusTx.Builtins (appendByteString, divideInteger, sha2_256, subtractInteger)
import PlutusTx.List qualified as List
import PlutusTx.Prelude (length)
import Prelude hiding (drop, length)
import Prelude qualified as Haskell

-- | A type for representing hash digests.
newtype Hash = Hash BuiltinByteString
  deriving (Haskell.Eq, Haskell.Show)

PlutusTx.unstableMakeIsData ''Hash

type Proof = [Either Hash Hash]

data PEitherData (a :: PType) (b :: PType) (s :: S)
  = PLeft (Term s (PDataRecord '["_0" ':= a]))
  | PRight (Term s (PDataRecord '["_0" ':= b]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType (PEitherData a b) where type DPTStrat _ = PlutusTypeData

instance (PLiftData a, PLiftData b) => PUnsafeLiftDecl (PEitherData a b) where
  type PLifted (PEitherData a b) = Either (PLifted a) (PLifted b)

deriving via
  (DerivePConstantViaData (Either a b) (PEitherData (PConstanted a) (PConstanted b)))
  instance
    (PConstantData a, PConstantData b) => PConstantDecl (Either a b)

type PProof = PBuiltinList (PEitherData PHash PHash)

phash :: Term s (PByteString :--> PHash)
phash = phoistAcyclic $ plam $ \vth -> pcon @PHash $ PHash $ psha2_256 # vth

pcombineHash :: Term s (PHash :--> PHash :--> PHash)
pcombineHash = phoistAcyclic $ plam $ \h h' -> h <> h'

instance Semigroup (Term s PHash) where
  x <> y = punsafeBuiltin PLC.AppendByteString # x # y

-- | Computes a SHA-256 hash of a given 'BuiltinByteString' message.
hash :: BuiltinByteString -> Hash
hash = Hash . sha2_256
{-# INLINEABLE hash #-}

{- | Combines two hashes digest into a new one. This is effectively a new hash
 digest of the same length.
-}
combineHash :: Hash -> Hash -> Hash
combineHash (Hash h) (Hash h') = hash (appendByteString h h')
{-# INLINEABLE combineHash #-}

pmember :: Term s (PByteString :--> PHash :--> PProof :--> PBool)
pmember = phoistAcyclic $ plam $ \e root proof -> go # root # (phash # e) # proof
  where
    go :: forall (s' :: S). Term s' (PHash :--> PHash :--> PProof :--> PBool)
    go = phoistAcyclic $
      pfix #$ plam $ \self root root' xs ->
        pelimList
          ( \y ys -> pmatch y $ \case
              PLeft l -> self # root # (pcombineHash # (pfield @"_0" # l) # root') # ys
              PRight r -> self # root # (pcombineHash # root' # (pfield @"_0" # r)) # ys
          )
          (root #== root')
          xs

member :: BuiltinByteString -> Hash -> Proof -> Bool
member e root = go (hash e)
  where
    go root' = \case
      [] -> root' == root
      Left l : q -> go (combineHash l root') q
      Right r : q -> go (combineHash root' r) q

mkProof :: BuiltinByteString -> MerkleTree -> Maybe Proof
mkProof e = go []
  where
    he = hash e
    go es = \case
      MerkleEmpty -> Nothing
      MerkleLeaf h _ ->
        if h == he
          then Just es
          else Nothing
      MerkleNode _ l r ->
        go (Right (rootHash r) : es) l <|> go (Left (rootHash l) : es) r

fromList :: [BuiltinByteString] -> MerkleTree
fromList es0 = recursively (length es0) es0
  where
    recursively len =
      \case
        [] ->
          MerkleEmpty
        [e] ->
          MerkleLeaf (hash e) e
        es ->
          let cutoff = len `divideInteger` 2
              (l, r) = (List.take cutoff es, drop cutoff es)
              lnode = recursively cutoff l
              rnode = recursively (len - cutoff) r
           in MerkleNode (combineHash (rootHash lnode) (rootHash rnode)) lnode rnode
{-# INLINEABLE fromList #-}

{- | Deconstruct a 'MerkleTree' back to a list of elements.

 >>> toList (fromList xs) == xs
 True
-}
toList :: MerkleTree -> [BuiltinByteString]
toList = go
  where
    go = \case
      MerkleEmpty -> []
      MerkleLeaf _ e -> [e]
      MerkleNode _ n1 n2 -> toList n1 <> toList n2
{-# INLINEABLE toList #-}

data MerkleTree
  = MerkleEmpty
  | MerkleNode Hash MerkleTree MerkleTree
  | MerkleLeaf Hash BuiltinByteString
  deriving (Haskell.Show, Haskell.Eq)

rootHash :: MerkleTree -> Hash
rootHash = \case
  MerkleEmpty -> hash ""
  MerkleLeaf h _ -> h
  MerkleNode h _ _ -> h

drop :: Integer -> [a] -> [a]
drop n rs | n <= 0 = rs
drop n (_ : xs) = drop (subtractInteger n 1) xs
drop _ [] = []
{-# INLINEABLE drop #-}

newtype PHash (s :: S) = PHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)

instance DerivePlutusType PHash where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PHash where
  type PLifted PHash = Hash

deriving via
  (DerivePConstantViaBuiltin Hash PHash PByteString)
  instance
    (PConstantDecl Hash)

data PMerkleTree (s :: S)
  = PMerkleEmpty (Term s (PDataRecord '[]))
  | PMerkleNode (Term s (PDataRecord '["_0" ':= PHash, "left" ':= PMerkleTree, "right" ':= PMerkleTree]))
  | PMerkleLeaf (Term s (PDataRecord '["hash" ':= PHash, "_1" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PMerkleTree where type DPTStrat _ = PlutusTypeData
