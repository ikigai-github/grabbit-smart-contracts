module Vulcan.Onchain.Metadata.Constants where

import Plutarch
import Plutarch.Api.V2 (PTokenName (PTokenName), PTxOutRef)
import Plutarch.Prelude 

phashTxRef :: Term s (PTxOutRef :--> PByteString)
phashTxRef = phoistAcyclic $
  plam $ \oref ->
    pletFields @["id", "idx"] oref $ \orefFields ->
        psha2_256 #$ pconsBS # orefFields.idx # (pfield @"_0" # orefFields.id)


