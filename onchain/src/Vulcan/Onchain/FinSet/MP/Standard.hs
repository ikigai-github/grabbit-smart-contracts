{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Vulcan.Onchain.FinSet.MP.Standard (
  mkNodeMP,
  pNodeMP,
  nodeMP,
) where

import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P

import PlutusLedgerApi.V2 (
  CurrencySymbol,
  MintingPolicy,
  TxOutRef,
  ValidatorHash,
 )

import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Onchain.FinSet.MP.Common (
  PFinSetCommon (mint, ownCS),
  makeCommon,
  pDeinit,
  pInit,
  pInsert,
  pRemove,
  pRemoveAndDeinit,
 )
import Vulcan.Onchain.FinSet.MP.Helpers (
  checkStateMinted,
  hasUtxoWithRef,
 )
import Vulcan.Types.FinSet (PNodeAction (..))
import Vulcan.Utils (
  passert,
  pfieldh,
  pverifyDataC,
  toPlutarchConfig,
  wrapMintingPolicy,
 )

--------------------------------
-- FinSet Node Minting Policy:
--------------------------------

mkNodeMP ::
  Vulcan.Config ->
  CurrencySymbol ->
  ValidatorHash ->
  ClosedTerm
    ( PTxOutRef
        :--> PNodeAction
        :--> PScriptContext
        :--> PUnit
    )
mkNodeMP cfg marketCS nodeVal = plam $ \oref redm ctx -> P.do
  (common, inputs, insAsOuts, refInsAsOuts, _, _, _, _) <-
    runTermCont $
      makeCommon cfg marketCS nodeVal ctx

  pmatch redm $ \case
    PInit _ -> P.do
      passert cfg "Init must consume TxOutRef" $
        hasUtxoWithRef # oref # inputs
      pInit cfg common
    ---------------------
    otherRedeemers -> P.do
      -- StateMP must validate every Tx except Init
      passert cfg "All subsequent node actions must mint/burn State" $
        checkStateMinted cfg # common.ownCS # common.mint # refInsAsOuts # insAsOuts

      case otherRedeemers of
        PDeinit _ -> pDeinit cfg common
        PInsert action -> P.do
          act <- pletFields @'["keyToInsert", "coveringNode"] action
          pInsert cfg common # act.keyToInsert # act.coveringNode
        PRemove action -> P.do
          act <- pletFields @'["keyToRemove", "coveringNode"] action
          pRemove cfg common # act.keyToRemove # act.coveringNode
        PRemoveAndDeinit (pfieldh @"finalKey" -> pk) -> P.do
          pRemoveAndDeinit cfg common # pk

------------------------------------------
-- Scripts

pNodeMP ::
  Vulcan.Config ->
  CurrencySymbol ->
  ValidatorHash ->
  ClosedTerm (PTxOutRef :--> PMintingPolicy)
pNodeMP config pmarketCS nodeVH =
  plam $ \oref redm' ctx -> unTermCont $ do
    redm <- pverifyDataC @PNodeAction redm'
    pure . popaque $
      mkNodeMP config pmarketCS nodeVH
        # oref
        # pfromData redm
        # ctx

-- | Compile the Finite Set Node Minting Policy
nodeMP :: Vulcan.Config -> CurrencySymbol -> ValidatorHash -> TxOutRef -> MintingPolicy
nodeMP config pmarketCS nodeVH oref =
  wrapMintingPolicy (toPlutarchConfig config) $
    mkNodeMP config pmarketCS nodeVH
      # pconstant oref
