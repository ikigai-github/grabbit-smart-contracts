module Vulcan.Onchain.Metadata.Constants where

import Plutarch
import Plutarch.Api.V1 (PTokenName)
import Plutarch.Lift (pconstant)
import PlutusLedgerApi.V1 (TokenName)
import Data.ByteString (ByteString)

-- Reference NFT Label
label100 :: Term s PByteString
label100 = phexByteStr "000643b0" -- 6

-- NFT Label
label222 :: Term s PByteString
label222 = phexByteStr "000de140" -- 13

-- FT Label
label333 :: Term s PByteString
label333 = phexByteStr "0014df10" -- 20

-- Royalty Label
label500 :: Term s PByteString
label500 = phexByteStr "001f4d70" -- 31

royaltyTN :: Term s PTokenName
royaltyTN =
  let tn :: TokenName
      tn = "Royalty"
   in pconstant tn

ownershipTN :: Term s PTokenName
ownershipTN =
  let tn :: TokenName
      tn = "Ownership"
   in pconstant tn

laneTN :: Term s PTokenName
laneTN =
  let tn :: TokenName
      tn = "Lane "
   in pconstant tn

-- pbreakTokenNameWith :: Term s PTokenName -> Term s PTokenName -> Term s (PPair PByteString PByteString)
-- pbreakTokenNameWith prefix tn =
--   plet (pto tn) $ \tnBS ->
--     plet (plengthBS # pto prefix) $ \prefixLength ->
--     pcon $ PPair (psliceBS # 0 # prefixLength # tnBS) (psliceBS # prefixLength # (plengthBS # tnBS) # tnBS)

pbreakTokenNameAt :: Term s PTokenName -> Term s PInteger -> Term s (PPair PByteString PByteString)
pbreakTokenNameAt tn i =
  plet (pto tn) $ \tnBS ->
    pcon $ PPair (psliceBS # 0 # i # tnBS) (psliceBS # i # (plengthBS # tnBS) # tnBS)

pbreakTokenName :: Term s PTokenName -> Term s (PPair PByteString PByteString)
pbreakTokenName tn =
  plet (pto tn) $ \tnBS ->
    pcon $ PPair (psliceBS # 0 # 4 # tnBS) (psliceBS # 4 # (plengthBS # tnBS) # tnBS)

psliceLabel :: Term s PTokenName -> Term s PByteString
psliceLabel tn =
  let tnBS = pto tn
   in psliceBS # 0 # 4 # tnBS

pisPrefixOf :: Term s (PByteString :--> PTokenName :--> PBool)
pisPrefixOf = phoistAcyclic $
  plam $ \prefix tn ->
    let tnBS = pto tn
     in psliceBS # 0 # (plengthBS # prefix) # tnBS #== prefix

pisPrefixedWithLabel :: Term s (PByteString :--> PTokenName :--> PBool)
pisPrefixedWithLabel = phoistAcyclic $
  plam $ \label tn ->
    let tnBS = pto tn
     in psliceBS # 0 # 4 # tnBS #== label

pisNFTLabel :: Term s (PTokenName :--> PBool)
pisNFTLabel = phoistAcyclic $
  plam $ \tn ->
    let tnBS = pto tn
     in psliceBS # 0 # 4 # tnBS #== label222
