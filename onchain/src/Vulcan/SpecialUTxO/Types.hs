{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vulcan.SpecialUTxO.Types (
  PUTxO (..),
  NamedToken (..),
  SpecialUTxOTag (..),
  mkUTxO,
  boughtTN,
  pnodeKeyTN,
  pcorrNodeTN,
  poriginNodeTN,
  pnodeKeyToken,
  pparseNodeKey,
) where

import Plutarch.Api.V1 (
  AmountGuarantees (NonZero, Positive),
  KeyGuarantees (Sorted),
  PCurrencySymbol,
  PPubKeyHash,
  PTokenName (..),
  PValue,
 )
import Plutarch.Api.V1.Value qualified as V
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PConstanted, PUnsafeLiftDecl (PLifted))
import Plutarch.Monadic qualified as P

import Data.ByteString qualified as BS
import Vulcan.Common.Types.FinSet (corrNodePrefix, setNodePrefix)
import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Common.Types.SpecialUtxo (
  UTxO,
 )
import Vulcan.Types.Auction (
  PAuctionEscrow,
  PBidEscrow,
  PRegistrationEscrow,
 )
import Vulcan.Types.Market (PMarketTerms)
import Vulcan.Utils (passert, pisPrefixOf)

------------------------------------
-- Auction UTxOs:

data PUTxO (t :: PType) (s :: S)
  = PUTxO
      ( Term
          s
          ( PDataRecord
              '[ "datum" ':= t
               , "extraValue" ':= PValue 'Sorted 'Positive
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType (PUTxO t) where type DPTStrat _ = PlutusTypeData

instance PLiftData t => PUnsafeLiftDecl (PUTxO t) where
  type PLifted (PUTxO t) = UTxO (PLifted t)
deriving via
  (DerivePConstantViaData (UTxO t) (PUTxO (PConstanted t)))
  instance
    PConstantData t => PConstantDecl (UTxO t)

-- | Constructor for PUTxO datatype
mkUTxO ::
  forall (t :: PType).
  PIsData t =>
  ClosedTerm
    ( PAsData t
        :--> PValue 'Sorted 'Positive
        :--> PUTxO t
    )
mkUTxO = phoistAcyclic $
  plam $ \datum val ->
    pcon $
      PUTxO $
        pdcons @"datum" # datum
          #$ pdcons @"extraValue" # pdata val
          #$ pdnil

data SpecialUTxOTag (t :: PType) where
  BidUTxO :: SpecialUTxOTag PBidEscrow
  RegUTxO :: SpecialUTxOTag PRegistrationEscrow

class NamedToken (t :: PType) where
  pnameToken :: Term s PTokenName
instance NamedToken PAuctionEscrow where
  pnameToken = pconstant "AuctionEscrow"
instance NamedToken PRegistrationEscrow where
  pnameToken = pconstant "RegistrationEscrow"
instance NamedToken PMarketTerms where
  pnameToken = pconstant "MarketEscrow"
instance NamedToken PBidEscrow where
  pnameToken = pconstant "BidEscrow"

boughtTN :: Term s PTokenName
boughtTN = pconstant "BoughtEscrow"

------------------------------------
-- Set Node UTxO:

-- | Prefix of the SetNode token TokenName
psetNodePrefix :: ClosedTerm PByteString
psetNodePrefix = pconstant setNodePrefix

setNodePrefixLength :: Integer
setNodePrefixLength = fromIntegral $ BS.length setNodePrefix

-- | Prefix of the Corresponding Node token TokenName
pcorrNodePrefix :: ClosedTerm PByteString
pcorrNodePrefix = pconstant corrNodePrefix

{- splices a node key to the setNode prefix to create a tokenName
  when provided a the key.
-}
pnodeKeyTN :: ClosedTerm (PByteString :--> PTokenName)
pnodeKeyTN = phoistAcyclic $
  plam $ \nodeKey -> pcon $ PTokenName $ psetNodePrefix <> nodeKey

-- | The TokenName for the Origin Node Token.
poriginNodeTN :: ClosedTerm PTokenName
poriginNodeTN = pcon $ PTokenName psetNodePrefix

{- | The TokenName for the Corresponding Node Token.
    There is only 1 Corresponding Node Token stored in AuctionEscrow UTxO.
-}
pcorrNodeTN :: ClosedTerm PTokenName
pcorrNodeTN = pcon $ PTokenName pcorrNodePrefix

{- parses the underlying ByteString from a TokenName. Will return nothing
  if it is the origin Node and will fail if the TokenName is not prefixed
  with the correct TokenName prefix for the nodes.
-}
pparseNodeKey :: Vulcan.Config -> ClosedTerm (PTokenName :--> PMaybe PByteString)
pparseNodeKey cfg = phoistAcyclic $
  plam $ \(pto -> tn) -> P.do
    let prefixLength = pconstant setNodePrefixLength
        tnLength = plengthBS # tn
        key = psliceBS # prefixLength # (tnLength - prefixLength) # tn
    passert cfg "incorrect node prefix" $ pisPrefixOf # psetNodePrefix # tn
    pif (prefixLength #< tnLength) (pcon $ PJust key) (pcon PNothing)

-- | Creates a certain amount of SetNode tokens for certain PubKeyHash
pnodeKeyToken ::
  ClosedTerm
    ( PCurrencySymbol
        :--> PAsData PPubKeyHash
        :--> PInteger
        :--> PValue 'Sorted 'NonZero
    )
pnodeKeyToken = phoistAcyclic $
  plam $ \nodeCS (pto . pfromData -> key) amount ->
    V.psingleton # nodeCS # (pnodeKeyTN # key) # amount
