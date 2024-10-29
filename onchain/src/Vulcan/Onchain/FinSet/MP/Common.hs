module Vulcan.Onchain.FinSet.MP.Common (
  PFinSetCommon (..),
  makeCommon,
  pInit,
  pDeinit,
  pRemove,
  pInsert,
  pRemoveAndDeinit,
) where

import Plutarch.Api.V2 (
  AmountGuarantees (NonZero),
  KeyGuarantees (Sorted),
  PCurrencySymbol,
  PInterval,
  PPOSIXTime,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PMinting),
  PTxInInfo,
  PTxOut,
  PValue,
 )
import Plutarch.Monadic qualified as P

import Plutarch.Api.V1.Value (pnormalize)
import PlutusLedgerApi.V2 (CurrencySymbol, ValidatorHash)
import Vulcan.Common.Types.Instances qualified as Vulcan

import Vulcan.Onchain.FinSet.MP.Helpers (
  correctNodeTokenMinted,
  correctNodeTokensMinted,
  coversKey,
 )
import Vulcan.SpecialUTxO.Types (pcorrNodeTN, pnodeKeyTN, poriginNodeTN)
import Vulcan.SpecialUTxO.Utils (
  marketInputUtxoDatum,
  nodeInputUtxoDatum,
  parseNodeOutputUtxo,
 )
import Vulcan.Types.Auction (PPositive)
import Vulcan.Types.FinSet (
  PSetNode,
  asPredecessorOf,
  asSuccessorOf,
  isEmptySet,
  isFirstNode,
  isLastNode,
 )
import Vulcan.Utils (passert, patScriptAddress, pfindWithRest, pmapMaybe, ptryFirstJust)

makeCommon ::
  forall {r :: PType} {s :: S}.
  Vulcan.Config ->
  CurrencySymbol ->
  ValidatorHash ->
  Term s PScriptContext ->
  TermCont @r
    s
    ( PFinSetCommon s
    , Term s (PBuiltinList PTxInInfo)
    , Term s (PBuiltinList PTxOut)
    , Term s (PBuiltinList PTxOut)
    , Term s (PBuiltinList PTxOut)
    , Term s (PBuiltinList (PAsData PPubKeyHash))
    , Term s (PInterval PPOSIXTime)
    , Term s PPositive
    )
makeCommon cfg marketCS nodeVal ctx' = do
  ------------------------------
  -- Preparing info needed for validation:
  ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  info <-
    tcont $
      pletFields
        @'["inputs", "outputs", "mint", "referenceInputs", "signatories", "validRange"]
        ctx.txInfo
  ownCS <- tcont . plet $ P.do
    PMinting mintRecord <- pmatch $ ctx.purpose
    pfromData $ pfield @"_0" # mintRecord

  mint <- tcont . plet $ pnormalize #$ pfromData info.mint
  asOuts <- tcont . plet $ pmap # plam (pfield @"resolved" #)
  refInsAsOuts <- tcont . plet $ asOuts # pfromData info.referenceInputs
  insAsOuts <- tcont . plet $ asOuts # pfromData info.inputs
  onlyAtNodeVal <- tcont . plet $ pfilter #$ patScriptAddress # pconstant nodeVal
  let fromNodeValidator = onlyAtNodeVal # insAsOuts
      toNodeValidator = onlyAtNodeVal # info.outputs
  ------------------------------
  -- to get market min ADA we use reference input on Market UTxO
  -- min ADA needed for making expectations about node ADA value size
  minAda <- tcont . plet $ P.do
    let pmarketCS = pconstant marketCS
    market <- plet $ ptryFirstJust # (marketInputUtxoDatum # pmarketCS) # refInsAsOuts
    pfromData $ pfield @"minAda" # market

  nodeInputs <- tcont . plet $ pmapMaybe # (nodeInputUtxoDatum # ownCS) # fromNodeValidator
  nodeOutputs <-
    tcont . plet $
      pmapMaybe
        # (parseNodeOutputUtxo cfg # ownCS # minAda)
        # toNodeValidator

  let common =
        MkCommon
          { ownCS
          , mint
          , nodeInputs
          , nodeOutputs
          }

  pure
    ( common
    , info.inputs
    , insAsOuts
    , refInsAsOuts
    , info.outputs
    , pfromData info.signatories
    , pfromData info.validRange
    , minAda
    )

pInit :: forall (s :: S). Vulcan.Config -> PFinSetCommon s -> Term s PUnit
pInit cfg common = P.do
  -- Input Checks
  passert cfg "Init must not spend Nodes" $ pnull # common.nodeInputs
  -- Output Checks:
  PPair _ otherNodes <-
    pmatch $
      pfindWithRest # isEmptySet # common.nodeOutputs
  passert cfg "Init output exactly one Node" $
    pnull # otherNodes
  -- Mint checks:
  let tokenNames = pcons # pcorrNodeTN #$ pcons # poriginNodeTN # pnil

  passert cfg "Incorrect amount of minted node tokens for Init" $
    correctNodeTokensMinted # common.ownCS # tokenNames # 1 # common.mint

  pconstant ()

pDeinit :: forall s. Vulcan.Config -> PFinSetCommon s -> Term s PUnit
pDeinit cfg common = P.do
  -- Input Checks
  PPair _ otherNodes <- pmatch $ pfindWithRest # isEmptySet # common.nodeInputs
  passert cfg "Deinit must spend exactly one node" $ pnull # otherNodes
  -- Output Checks:
  passert cfg "Deinit must not output nodes" $ pnull # common.nodeOutputs
  -- Mint checks:
  let tokenNames = pcons # pcorrNodeTN #$ pcons # poriginNodeTN # pnil

  passert cfg "Incorrect amount of burnt node tokens for Deinit" $
    correctNodeTokensMinted # common.ownCS # tokenNames # (-1) # common.mint

  pconstant ()

pInsert ::
  forall (s :: S).
  Vulcan.Config ->
  PFinSetCommon s ->
  Term s (PAsData PPubKeyHash :--> PAsData PSetNode :--> PUnit)
pInsert cfg common = plam $ \pkToInsert node -> P.do
  keyToInsert <- plet . pto . pfromData $ pkToInsert
  passert cfg "Node should cover inserting key" $
    coversKey # node # keyToInsert
  -- Input Checks
  PPair _ otherNodes <-
    pmatch $
      pfindWithRest # plam (node #==) # common.nodeInputs
  passert cfg "Insert must spend exactly one node" $
    pnull # otherNodes
  -- Output Checks:
  let prevNodeOutDatum = pdata $ asPredecessorOf # node # keyToInsert
      nodeOutDatum = pdata $ asSuccessorOf # keyToInsert # node
  hasDatumInOutputs <- plet $
    plam $ \datum ->
      pany # plam (datum #==) # common.nodeOutputs
  passert cfg "Incorrect node outputs for Insert" $
    hasDatumInOutputs # prevNodeOutDatum
      #&& hasDatumInOutputs # nodeOutDatum
  -- Mint checks:
  passert cfg "Incorrect mint for Insert" $
    correctNodeTokenMinted # common.ownCS # (pnodeKeyTN # keyToInsert) # 1 # common.mint

  pconstant ()

pRemove ::
  forall (s :: S).
  Vulcan.Config ->
  PFinSetCommon s ->
  Term s (PAsData PPubKeyHash :--> PAsData PSetNode :--> PUnit)
pRemove cfg common = plam $ \pkToRemove node -> P.do
  keyToRemove <- plet . pto . pfromData $ pkToRemove
  passert cfg "Node does not cover key to remove" $
    coversKey # node # keyToRemove
  -- Input Checks
  let prevNodeInDatum = pdata $ asPredecessorOf # node # keyToRemove
      nodeInDatum = pdata $ asSuccessorOf # keyToRemove # node
  findNodeInDatumInRest <- plet $
    plam $ \datum inputs ->
      pfindWithRest
        # plam (datum #==)
        # inputs
  PPair _ rest <- pmatch $ findNodeInDatumInRest # prevNodeInDatum # common.nodeInputs
  PPair _ extraNodes <- pmatch $ findNodeInDatumInRest # nodeInDatum # rest
  passert cfg "Remove must spend exactly two nodes" $
    pnull # extraNodes
  -- Output Checks:

  {- This check has weak constraints due to the fact that the only way
    To provide more node outputs would be to mint more node tokens.
    Therefore we can safely assure that this is the only node Output.

    Error is more explicit simply for debugging
  -}
  passert cfg "There must be exactly one output with update node" $
    pany # plam (node #==) # common.nodeOutputs

  passert cfg "Incorrect mint for Remove" $
    correctNodeTokenMinted # common.ownCS # (pnodeKeyTN # keyToRemove) # (-1) # common.mint

  pconstant ()

pRemoveAndDeinit ::
  forall (s :: S).
  Vulcan.Config ->
  PFinSetCommon s ->
  Term s (PAsData PPubKeyHash :--> PUnit)
pRemoveAndDeinit cfg common = plam $ \finakPK -> P.do
  finalKey <- plet . pto . pfromData $ finakPK
  -- Input Checks
  findNodeWithPk <- plet $
    plam $ \isNthNode inputs ->
      pfindWithRest
        # plam (\node -> isNthNode # finalKey # pfromData node)
        # inputs
  PPair _ otherNodes <- pmatch $ findNodeWithPk # isFirstNode # common.nodeInputs
  PPair _ extraNodes <- pmatch $ findNodeWithPk # isLastNode # otherNodes
  passert cfg "RemoveAndDeinit must spend exactly two nodes" $ pnull # extraNodes
  -- Output Checks:
  passert cfg "RemoveAndDeinit must not output nodes" $ pnull # common.nodeOutputs
  -- Mint Checks:
  let tokenNames =
        pcons # (pnodeKeyTN # finalKey) #$ pcons # poriginNodeTN #$ pcons # pcorrNodeTN # pnil

  passert cfg "Incorrect amount of burnt node tokens for Deinit" $
    correctNodeTokensMinted # common.ownCS # tokenNames # (-1) # common.mint

  pconstant ()

-- Common information shared between all redeemers.
data PFinSetCommon (s :: S) = MkCommon
  { ownCS :: Term s PCurrencySymbol
  -- ^ state token (own) CS
  , mint :: Term s (PValue 'Sorted 'NonZero)
  -- ^ value minted in current Tx
  , nodeInputs :: Term s (PBuiltinList (PAsData PSetNode))
  -- ^ current Tx outputs to AuctionValidator
  , nodeOutputs :: Term s (PBuiltinList (PAsData PSetNode))
  -- ^ current Tx inputs
  }
  deriving stock (Generic)
