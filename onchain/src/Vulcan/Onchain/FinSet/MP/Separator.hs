module Vulcan.Onchain.FinSet.MP.Separator (
  mkSepNodeMP,
  pSepNodeMP,
  sepNodeMP,
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

import Plutarch.Extra.Interval (pafter, pbefore)
import Plutarch.Positive (ptryPositive)
import Vulcan.Common.Types.FinSet (SeparatorConfig)
import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Onchain.FinSet.MP.Common (
  PFinSetCommon (mint, nodeInputs, nodeOutputs, ownCS),
  makeCommon,
  pDeinit,
  pInit,
  pInsert,
  pRemove,
  pRemoveAndDeinit,
 )
import Vulcan.Onchain.FinSet.MP.Helpers (
  checkStateMinted,
  correctPaidToPK,
  coversSeparators,
  hasUtxoWithRef,
  pseparatorsMintValue,
 )
import Vulcan.Types.FinSet (
  PSepNodeAction (..),
  PSeparatorConfig,
  PSetNode,
  asPredecessorOf,
  asSuccessorOf,
  mkBSNode,
 )
import Vulcan.Utils (
  padaTolovelace,
  passert,
  pfieldh,
  pfindWithRest,
  plovelace,
  ptxSignedBy,
  pverifyDataC,
  toPlutarchConfig,
  wrapMintingPolicy,
 )

--------------------------------
-- FinSet Node Minting Policy:
--------------------------------

mkSepNodeMP ::
  Vulcan.Config ->
  CurrencySymbol ->
  ValidatorHash ->
  ClosedTerm
    ( PTxOutRef
        :--> PSeparatorConfig
        :--> PSepNodeAction
        :--> PScriptContext
        :--> PUnit
    )
mkSepNodeMP cfg marketCS nodeVal = plam $ \oref separators redm ctx -> P.do
  (common, inputs, insAsOuts, refInsAsOuts, outputs, signatures, validityRange, minAda) <-
    runTermCont $
      makeCommon cfg marketCS nodeVal ctx
  sep <- pletFields @'["signer", "cutOff"] separators

  pmatch redm $ \case
    PSepInit _ -> P.do
      passert cfg "Init must consume TxOutRef" $
        hasUtxoWithRef # oref # inputs
      pInit cfg common
    ---------------------
    PInsertSeps action -> P.do
      act <- pletFields @'["separators", "coveringNode"] action
      passert cfg "must be signed by separator address" $
        ptxSignedBy # signatures # sep.signer

      passert cfg "Separators can only be added before cutOff time" $
        pafter # pfromData sep.cutOff # validityRange

      pInsertSeps cfg common # pfromData act.separators # act.coveringNode
    PRemoveSeps action -> P.do
      act <- pletFields @'["separators", "coveringNode"] action
      let refund = minAda * (ptryPositive #$ plength #$ pfromData act.separators)
      passert cfg "coveringNode Ada not refunded" $
        pany
          # (correctPaidToPK # sep.signer # (plovelace #$ padaTolovelace # refund))
          # outputs
      passert cfg "Separators can only be removed after cutOff time" $
        pbefore # pfromData sep.cutOff # validityRange
      pRemoveSeps cfg common # act.separators # act.coveringNode
    otherRedeemers -> P.do
      -- StateMP must validate every Tx except Init and Separator Txs
      passert cfg "All subsequent coveringNode actions must mint/burn State" $
        checkStateMinted cfg # common.ownCS # common.mint # refInsAsOuts # insAsOuts

      case otherRedeemers of
        PSepDeinit _ -> pDeinit cfg common
        PSepInsert action -> P.do
          act <- pletFields @'["keyToInsert", "coveringNode"] action
          pInsert cfg common # act.keyToInsert # act.coveringNode
        PSepRemove action -> P.do
          act <- pletFields @'["keyToRemove", "coveringNode"] action
          pRemove cfg common # act.keyToRemove # act.coveringNode
        PSepRemoveAndDeinit (pfieldh @"finalKey" -> pk) -> P.do
          pRemoveAndDeinit cfg common # pk

------------------------------------------
-- Scripts

pInsertSeps ::
  forall s.
  Vulcan.Config ->
  PFinSetCommon s ->
  Term s (PBuiltinList (PAsData PByteString) :--> PAsData PSetNode :--> PUnit)
pInsertSeps cfg common = plam $ \separators coveringNode -> P.do
  suffixedSeparators <- plet $ pmap # plam ((<> pconstant "Sep") . pfromData) # separators

  passert cfg "Node does not cover separators" $
    coversSeparators # coveringNode # suffixedSeparators

  -- Input Checks:
  PPair _ otherNodes <-
    pmatch $
      pfindWithRest # plam (coveringNode #==) # common.nodeInputs
  passert cfg "Incorrect coveringNode inputs" $
    pnull # otherNodes

  -- Output Checks
  passert cfg "Incorrect number of nodes out" $
    expectedNodes # coveringNode # suffixedSeparators #== common.nodeOutputs

  passert cfg "Incorrect number of separators minted" $
    common.mint #== pseparatorsMintValue # 1 # common.ownCS # suffixedSeparators
  pconstant ()

pRemoveSeps ::
  forall s.
  Vulcan.Config ->
  PFinSetCommon s ->
  Term s (PBuiltinList (PAsData PByteString) :--> PAsData PSetNode :--> PUnit)
pRemoveSeps cfg common = plam $ \separators coveringNode -> P.do
  suffixedSeparators <- plet $ pmap # plam ((<> pconstant "Sep") . pfromData) # separators

  passert cfg "Node does not cover separators" $
    coversSeparators # coveringNode # suffixedSeparators

  -- Input Checks
  passert cfg "Incorrect nodes in" $
    expectedNodes # coveringNode # suffixedSeparators #== common.nodeInputs

  -- Ouput Checks:
  passert cfg "Incorrect coveringNode outputs" $
    pany # plam (coveringNode #==) # common.nodeOutputs

  passert cfg "Incorrect mint" $
    common.mint #== pseparatorsMintValue # (-1) # common.ownCS # suffixedSeparators

  pconstant ()

expectedNodes ::
  ClosedTerm
    ( PAsData PSetNode
        :--> PBuiltinList PByteString
        :--> PBuiltinList (PAsData PSetNode)
    )
expectedNodes = phoistAcyclic $
  plam $ \coveringNode suffixedSeparators -> P.do
    let firstOutDatum = asPredecessorOf # coveringNode #$ phead # suffixedSeparators
        head = pcons # pdata firstOutDatum # pnil
        toFinSetNodes :: Term _ (PBuiltinList PByteString :--> PBuiltinList (PAsData PSetNode))
        toFinSetNodes =
          precList
            ( \self x xs ->
                pcons
                  # pif
                    (pnull # xs)
                    (pdata $ asSuccessorOf # x # coveringNode)
                    (mkBSNode # x # (phead # xs))
                  # (self # xs)
            )
            (const pnil)
    pconcat # head # (toFinSetNodes # suffixedSeparators)

pSepNodeMP ::
  Vulcan.Config ->
  CurrencySymbol ->
  ValidatorHash ->
  ClosedTerm (PTxOutRef :--> PSeparatorConfig :--> PMintingPolicy)
pSepNodeMP config pmarketCS nodeVH =
  plam $ \oref separators redm' ctx -> unTermCont $ do
    redm <- pverifyDataC @PSepNodeAction redm'
    pure . popaque $
      mkSepNodeMP config pmarketCS nodeVH
        # oref
        # separators
        # pfromData redm
        # ctx

-- | Compile the Finite Set Node Minting Policy
sepNodeMP :: Vulcan.Config -> CurrencySymbol -> ValidatorHash -> TxOutRef -> SeparatorConfig -> MintingPolicy
sepNodeMP config pmarketCS nodeVH oref separators =
  wrapMintingPolicy (toPlutarchConfig config) $
    mkSepNodeMP config pmarketCS nodeVH
      # pconstant oref
      # pconstant separators
