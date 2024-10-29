module Vulcan.Utils (
  -- Scripts
  wrapMintingPolicy,
  wrapMintingPolicy',
  wrapValidator,
  wrapValidator',
  wrapMintingPolicyWith,
  wrapValidatorWith,
  pverifyDataC,
  -- List
  ptryFind,
  pfind,
  ppartitionWith,
  pconvertWithRest,
  pfindWithRest,
  pmapMaybe,
  ptryFirstJust,
  pfirstJust,
  ppartition,
  -- Value
  phasCS,
  pvalueOf,
  psumData,
  pposSingleton,
  plovelace,
  pgetCS,
  pisPrefixOf,
  padaTolovelace,
  adaTolovelace,
  pfindCurrencySymbolsByTokenPrefix,
  ppositiveSingleton,
  pcontainsValue,
  (#-),
  -- Patterns,
  pfieldh,
  pfield0,
  pfield1,
  -- Utils
  pcheck,
  pbindMaybe,
  psatisfies,
  passert,
  passertManually,
  percentage,
  traceError,
  pabs,
  pdatum,
  toPlutarchConfig,
  pfindOwnInput,
  pfromScriptAddress,
  patScriptAddress,
  tryPkFromAddress,
  ptoPkhAddress,
  ptoAddress,
  ptxSignedBy,
  ptryOwnHash,
  passumeWellFormedDatum,
  hasValueAndRef,
  passumeWellFormedInlineDatum,
) where

import Vulcan.Utils.List (
  pconvertWithRest,
  pfindWithRest,
  pfirstJust,
  pmapMaybe,
  ppartition,
  ppartitionWith,
  ptryFind,
  ptryFirstJust,
 )
import Vulcan.Utils.Prelude (
  pabs,
  passert,
  passertManually,
  pbindMaybe,
  pcheck,
  percentage,
  psatisfies,
  traceError,
 )
import Vulcan.Utils.Scripts (
  pverifyDataC,
  wrapMintingPolicy,
  wrapMintingPolicy',
  wrapMintingPolicyWith,
  wrapValidator,
  wrapValidator',
  wrapValidatorWith,
 )
import Vulcan.Utils.Value (
  adaTolovelace,
  padaTolovelace,
  pcontainsValue,
  pfindCurrencySymbolsByTokenPrefix,
  pgetCS,
  phasCS,
  pisPrefixOf,
  plovelace,
  pposSingleton,
  ppositiveSingleton,
  psumData,
  (#-),
 )

import Vulcan.Utils.Patterns (
  pfield0,
  pfield1,
  pfieldh,
 )

import Plutarch (Config (Config), TracingMode (DoTracing, NoTracing))
import Plutarch.Api.V1 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PCredential (PPubKeyCredential, PScriptCredential),
  PValidatorHash,
  PValue,
 )
import Plutarch.Api.V1.Value (
  pvalueOf,
 )
import Plutarch.Api.V2 (
  PAddress,
  PDatum,
  POutputDatum (POutputDatum),
  PPubKeyHash,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import Vulcan.Common.Types.Instances qualified as Vulcan

toPlutarchConfig :: Vulcan.Config -> Config
toPlutarchConfig cfg =
  Config $ case cfg.tracingMode of
    Vulcan.NoTracing -> NoTracing
    Vulcan.DoTracing _ -> DoTracing

-- | Checks that UTxO has certain TxOutRef and contains Value
hasValueAndRef ::
  ClosedTerm
    ( PTxOutRef
        :--> PValue 'Sorted 'Positive
        :--> PTxInInfo
        :--> PBool
    )
hasValueAndRef = phoistAcyclic $
  plam $ \ref lot txIn -> P.do
    input <- pletFields @'["outRef", "resolved"] txIn
    let actualValue = pfromData $ pfield @"value" # input.resolved
    pcontainsValue # actualValue # lot
      #&& (pfromData input.outRef #== ref)

-------------------------
-- Datums:

{- | Unsafely coerce inline datum that TxOut contains.

  Make sure to not apply it to a datums that is not a PData.
  (i.e. have custom PTryFrom instead of anyclass-derived)
-}
passumeWellFormedInlineDatum ::
  forall (a :: PType).
  PTryFrom PData (PAsData a) =>
  ClosedTerm (PAsData POutputDatum :--> PAsData a)
passumeWellFormedInlineDatum =
  phoistAcyclic $
    plam $ \(pfromData -> datum) -> P.do
      POutputDatum datum <- pmatch datum
      passumeWellFormedDatum @a #$ pfield @"outputDatum" # datum

{- | Unsafely coerce datum.

  Make sure to not apply it to a datums that is not a PData.
  (i.e. have custom PTryFrom instead of anyclass-derived)
-}
passumeWellFormedDatum ::
  forall (a :: PType).
  PTryFrom PData (PAsData a) =>
  ClosedTerm (PAsData PDatum :--> PAsData a)
passumeWellFormedDatum = phoistAcyclic $ plam punsafeCoerce

-- | Tries to parse PDatum, may fail if it's malformed
pdatum ::
  forall (a :: PType).
  PTryFrom PData (PAsData a) =>
  ClosedTerm (PAsData PDatum :--> PAsData a)
pdatum = phoistAcyclic $ plam $ ptryFromData . pto . pfromData

-- | Tries to parse PData, may fail if it's malformed
ptryFromData ::
  forall (a :: PType) (s :: S).
  PTryFrom PData (PAsData a) =>
  Term s PData ->
  Term s (PAsData a)
ptryFromData = unTermCont . fmap fst . tcont . ptryFrom @(PAsData a)

{- | Taken from Plutarch extra for V2 scripts
  Takes Validator's own TxOutRef (from Tx purpose)
  and finds an input Tx currently spending.
-}
pfindOwnInput :: ClosedTerm (PBuiltinList PTxInInfo :--> PTxOutRef :--> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \inputs ownOutRef ->
    pfind # (matches # ownOutRef) # inputs
  where
    matches :: ClosedTerm (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        outref #== pfield @"outRef" # txininfo

---------------------
-- Addresses:

-- | Ensures a input comes from a specific Script Address
pfromScriptAddress :: ClosedTerm (PValidatorHash :--> PTxInInfo :--> PBool)
pfromScriptAddress = phoistAcyclic $
  plam $ \valHash input ->
    patScriptAddress # valHash #$ pfield @"resolved" # input

-- | Ensures a TxOut is located (came from / goes to) at a specific Script Address
patScriptAddress :: ClosedTerm (PValidatorHash :--> PTxOut :--> PBool)
patScriptAddress = phoistAcyclic $
  plam $ \valHash out ->
    let credential = pfield @"credential" #$ pfield @"address" # out
     in pmatch credential $ \case
          PScriptCredential inputAddressHash ->
            pfield @"_0" # inputAddressHash #== valHash
          _ -> pcon PFalse

-- | Partial function, will fail with error if not a Public key address
tryPkFromAddress :: ClosedTerm (PAsData PAddress :--> PAsData PPubKeyHash)
tryPkFromAddress = phoistAcyclic $
  plam $ \addr ->
    let credential = pfield @"credential" #$ addr
     in pmatch credential $ \case
          PPubKeyCredential inputAddressHash ->
            pfield @"_0" # inputAddressHash
          _ -> ptraceError "Not a pubkey address"

-- | Ensures a ouput goes to a specific PubKey Address
ptoPkhAddress :: ClosedTerm (PAsData PPubKeyHash :--> PTxOut :--> PBool)
ptoPkhAddress = phoistAcyclic $
  plam $ \pkHash out ->
    let credential = pfield @"credential" #$ pfield @"address" # out
     in pmatch credential $ \case
          PPubKeyCredential inputAddressHash ->
            pfield @"_0" # inputAddressHash #== pkHash
          _ -> pcon PFalse

-- | Ensures a ouput goes to a specific Address
ptoAddress :: ClosedTerm (PAsData PAddress :--> PTxOut :--> PBool)
ptoAddress = phoistAcyclic $
  plam $ \address out -> address #== pfield @"address" # out

-- | Ensures that a transaction is signed by a specific PubKeyHash
ptxSignedBy ::
  ClosedTerm
    ( PBuiltinList (PAsData PPubKeyHash)
        :--> PAsData PPubKeyHash
        :--> PBool
    )
ptxSignedBy = phoistAcyclic $
  plam $ \sigs pkh -> pelem # pkh # sigs

{- | Attempts to find the Validator's own Hash
  using list of own Inputs and a currently spending TxOutRef.

  May fail validation.
-}
ptryOwnHash ::
  ClosedTerm
    ( PBuiltinList PTxInInfo
        :--> (PTxOutRef :--> PValidatorHash)
    )
ptryOwnHash = phoistAcyclic $
  plam $ \inputs outRef ->
    pmatch (pfindOwnInput # inputs # outRef) $ \case
      PJust tx -> P.do
        let cred = pfield @"credential" #$ pfield @"address" #$ pfield @"resolved" # tx
        pmatch cred $ \case
          PScriptCredential hashRecord -> pfromData $ pfield @"_0" # hashRecord
          PPubKeyCredential _ -> ptraceError "not a Validator"
      PNothing ->
        ptraceError "no own inputs"
