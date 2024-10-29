module Vulcan.Onchain.NFT.MP (nftMP, pNftMP) where

import Plutarch.Api.V1 (
  PMintingPolicy,
  mkMintingPolicy,
 )
import Plutarch.Internal (Config (Config, tracingMode), TracingMode (NoTracing))
import Plutarch.Monadic qualified as P

import PlutusLedgerApi.V1 (MintingPolicy, TxOutRef)
import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Utils.Prelude (passert)

-- | Mints an NFT, consuming TxOutRef
pNftMP ::
  Vulcan.Config ->
  TxOutRef ->
  ClosedTerm PMintingPolicy
pNftMP cfg (pconstantData -> oref) = plam $ \_ ctx -> popaque $ P.do
  let inputs =
        pfromData $
          pfield @"inputs"
            #$ pfield @"txInfo"
            # ctx
  passert cfg "TxOutref Must be consumed" $
    pany
      # plam (\input -> oref #== (pfield @"outRef" # input))
      # inputs

  pconstant ()

------------------------------------------
-- Scripts

-- | Compile the NFT Minting Policy
nftMP {-Vulcan.Config ->-} :: TxOutRef -> MintingPolicy
nftMP ref =
  mkMintingPolicy Config {tracingMode = NoTracing} $
    pNftMP (Vulcan.Config $ Vulcan.DoTracing Vulcan.Verbose) ref
