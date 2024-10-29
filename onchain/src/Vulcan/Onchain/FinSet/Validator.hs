module Vulcan.Onchain.FinSet.Validator (
  finSetValidator,
  pFinSetValidator,
  nodeValidator,
) where

import Data.ByteString (ByteString)

import Plutarch.Api.V2 (
  PScriptPurpose (PSpending),
  PValidator,
  mkValidator,
 )
import Plutarch.Monadic qualified as P

import PlutusLedgerApi.V2 (Validator)
import Vulcan.Common.Types.FinSet (setNodePrefix)
import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Utils (
  passert,
  pfield0,
  pfindCurrencySymbolsByTokenPrefix,
  pfindOwnInput,
  toPlutarchConfig,
 )
import Vulcan.Utils.Value (pcontainsCurrencySymbols)

{- | The validator under which NodeMP locks it's Nodes.
  The only thing this validator should check:
    When spending Node - it's NodeMP must validate this Tx.

  For performance and simplicity reasons it doesn't allow
  to freely spend anything that is not even a Node.

  So accidentally locking funds under this validator may mean that those will be lost.
-}
pFinSetValidator ::
  Vulcan.Config ->
  ByteString ->
  ClosedTerm PValidator
pFinSetValidator cfg prefix = plam $ \_ _ ctx' -> popaque $ P.do
  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  info <- pletFields @'["inputs", "mint"] ctx.txInfo

  let ownInputValue = P.do
        PSpending (pfromData . pfield0 -> ref) <- pmatch ctx.purpose
        let inputs = pfromData info.inputs
        PJust input <- pmatch $ pfindOwnInput # inputs # ref
        pfromData $ pfield @"value" #$ pfield @"resolved" # input
      -- all those CSs has tokens that prefixed by Node prefix
      -- any of those can be actual Node CS
      potentialNodeCSs = pfindCurrencySymbolsByTokenPrefix # ownInputValue # pconstant prefix

  passert cfg "Must mint/burn for any FinSet input" $
    {- To prevent unauthorized spend of Node UTxOs (which contain tokens with node CS)
      we simply require every MP for those CSs to validate this Tx
      If there is actual Node CS -
        this requirement will force SetNode MP to validate this Tx
      If there is not -
        Node UTxO won't be spent.
    -}
    pcontainsCurrencySymbols # pfromData info.mint # potentialNodeCSs

  pconstant ()

------------------------------------------
-- Scripts

-- | Compile the Set Node Validator
finSetValidator :: Vulcan.Config -> ByteString -> Validator
finSetValidator config bs =
  mkValidator (toPlutarchConfig config) $
    pFinSetValidator config bs

-- | Default NodeValidator
nodeValidator :: Vulcan.Config -> Validator
nodeValidator cfg = finSetValidator cfg setNodePrefix
