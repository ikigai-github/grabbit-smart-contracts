{-# LANGUAGE UndecidableInstances #-}

module Vulcan.Onchain.Collections.Utils where

import Plutarch.Api.V1 (AmountGuarantees (Positive), KeyGuarantees (Sorted), PCredential (PPubKeyCredential, PScriptCredential), PValidatorHash)
import Plutarch.Api.V1.Value (padaSymbol, pvalueOf)
import Plutarch.Api.V2 (
  PAddress,
  PCurrencySymbol,
  PMap (PMap),
  PPubKeyHash,
  PTokenName,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
  PValue (..),
 )
import Plutarch.Bool (pand')
import "liqwid-plutarch-extra" Plutarch.Extra.List (plookupAssoc)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pmatchC)

data PTriple (a :: PType) (b :: PType) (c :: PType) (s :: S)
  = PTriple (Term s a) (Term s b) (Term s c)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PTriple a b c) where type DPTStrat _ = PlutusTypeScott

tcexpectJust :: forall r (a :: PType) (s :: S). Term s r -> Term s (PMaybe a) -> TermCont @r s (Term s a)
tcexpectJust escape ma = tcont $ \f -> pmatch ma $ \case
  PJust v -> f v
  PNothing -> escape

paysToAddress :: Term s (PAddress :--> PTxOut :--> PBool)
paysToAddress = phoistAcyclic $ plam $ \adr txOut -> adr #== (pfield @"address" # txOut)

paysValueToAddress ::
  Term s (PValue 'Sorted 'Positive :--> PAddress :--> PTxOut :--> PBool)
paysValueToAddress = phoistAcyclic $
  plam $ \val adr txOut ->
    pletFields @["address", "value"] txOut $ \txoFields ->
      txoFields.address #== adr #&& txoFields.value #== val

paysAtleastValueToAddress ::
  Term s (PValue 'Sorted 'Positive :--> PAddress :--> PTxOut :--> PBool)
paysAtleastValueToAddress = phoistAcyclic $
  plam $ \val adr txOut ->
    pletFields @["address", "value"] txOut $ \txoFields ->
      txoFields.address #== adr #&& txoFields.value #<= val

paysToCredential :: Term s (PValidatorHash :--> PTxOut :--> PBool)
paysToCredential = phoistAcyclic $
  plam $ \valHash txOut ->
    let txOutCred = pfield @"credential" # (pfield @"address" # txOut)
     in pmatch txOutCred $ \case
          PScriptCredential txOutValHash -> (pfield @"_0" # txOutValHash) #== valHash
          PPubKeyCredential _ -> (pcon PFalse)

paysToPubKey :: Term s (PPubKeyHash :--> PTxOut :--> PBool)
paysToPubKey = phoistAcyclic $
  plam $ \pkh txOut ->
    let txOutCred = pfield @"credential" # (pfield @"address" # txOut)
     in pmatch txOutCred $ \case
          PScriptCredential _ -> pconstant False
          PPubKeyCredential pkh' -> (pfield @"_0" # pkh') #== pkh

ptryOutputToAddress :: (PIsListLike list PTxOut) => Term s (list PTxOut :--> PAddress :--> PTxOut)
ptryOutputToAddress = phoistAcyclic $
  plam $ \outs target ->
    ( pfix #$ plam $ \self xs ->
        pelimList
          ( \txo txos ->
              pif (target #== (pfield @"address" # txo)) txo (self # txos)
          )
          perror
          xs
    )
      # outs

ptryOwnOutput :: (PIsListLike list PTxOut) => Term s (list PTxOut :--> PValidatorHash :--> PTxOut)
ptryOwnOutput = phoistAcyclic $
  plam $ \outs target ->
    ( pfix #$ plam $ \self xs ->
        pelimList
          ( \txo txos ->
              pmatch (pfield @"credential" # (pfield @"address" # txo)) $ \case
                PPubKeyCredential _ -> (self # txos)
                PScriptCredential ((pfield @"_0" #) -> vh) ->
                  pif (target #== vh) txo (self # txos)
          )
          perror
          xs
    )
      # outs

ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs

-- Get the head of the list if the list contains exactly one element, otherwise error.
pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList
      (pelimList (\_ _ -> ptraceError "List contains more than one element."))
      (ptraceError "List is empty.")
      xs

ptxSignedByPkh ::
  Term s (PAsData PPubKeyHash :--> PBuiltinList (PAsData PPubKeyHash) :--> PBool)
ptxSignedByPkh = pelem

psymbolValueOfHelper ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( (PInteger :--> PBool)
        :--> PCurrencySymbol
        :--> ( PValue keys amounts
                :--> PInteger
             )
    )
psymbolValueOfHelper =
  phoistAcyclic $
    plam $ \cond sym value'' -> unTermCont $ do
      PValue value' <- pmatchC value''
      PMap value <- pmatchC value'
      m' <-
        tcexpectJust
          0
          ( plookupAssoc
              # pfstBuiltin
              # psndBuiltin
              # pdata sym
              # value
          )
      PMap m <- pmatchC (pfromData m')
      pure $
        pfoldr
          # plam
            ( \x v ->
                plet (pfromData $ psndBuiltin # x) $ \q ->
                  pif
                    (cond # q)
                    (q + v)
                    v
            )
          # 0
          # m

-- | @since 1.0.0
ppositiveSymbolValueOf ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> (PValue keys amounts :--> PInteger))
ppositiveSymbolValueOf = phoistAcyclic $ psymbolValueOfHelper #$ plam (0 #<)

-- | @since 1.0.0
pnegativeSymbolValueOf ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> (PValue keys amounts :--> PInteger))
pnegativeSymbolValueOf = phoistAcyclic $ psymbolValueOfHelper #$ plam (#< 0)

-- | Probably more effective than `plength . pflattenValue`
pcountOfUniqueTokens ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PInteger)
pcountOfUniqueTokens = phoistAcyclic $
  plam $ \val ->
    let tokensLength = plam (\pair -> pmatch (pfromData $ psndBuiltin # pair) $ \(PMap tokens) -> plength # tokens)
     in pmatch val $ \(PValue val') ->
          pmatch val' $ \(PMap csPairs) -> pfoldl # plam (\acc x -> acc + (tokensLength # x)) # 0 # csPairs

pcountCS ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PInteger)
pcountCS = phoistAcyclic $
  plam $ \val ->
    pmatch val $ \(PValue val') ->
      pmatch val' $ \(PMap csPairs) ->
        plength # csPairs

pcountNonAdaCS ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PInteger)
pcountNonAdaCS =
  phoistAcyclic $
    let go :: Term _ (PInteger :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap keys PTokenName PInteger))) :--> PInteger)
        go = plet (pdata padaSymbol) $ \padaSymbolD ->
          pfix #$ plam $ \self n ->
            pelimList (\x xs -> pif (pfstBuiltin # x #== padaSymbolD) (self # n # xs) (self # (n + 1) # xs)) n
     in plam $ \val ->
          pmatch val $ \(PValue val') ->
            pmatch val' $ \(PMap csPairs) ->
              go # 0 # csPairs

pfirstTokenName ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PTokenName)
pfirstTokenName = phoistAcyclic $
  plam $ \val ->
    pmatch val $ \(PValue val') ->
      pmatch val' $ \(PMap csPairs) ->
        pmatch (pfromData (psndBuiltin # (phead # csPairs))) $ \(PMap tokens) ->
          pfromData $ pfstBuiltin # (phead # tokens)

ptryLookupValue ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PValue keys amounts
        :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
    )
ptryLookupValue = phoistAcyclic $
  plam $ \policyId val ->
    pmatch val $ \(PValue val') ->
      precList
        ( \self x xs ->
            pif
              (pfstBuiltin # x #== policyId)
              ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                  tokens
              )
              (self # xs)
        )
        (const perror)
        # pto val'

pfirstTokenNameWithCS ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PAsData PCurrencySymbol :--> PValue keys amounts :--> PTokenName)
pfirstTokenNameWithCS = phoistAcyclic $
  plam $ \policyId val ->
    pmatch val $ \(PValue val') ->
      precList
        ( \self x xs ->
            pif
              (pfstBuiltin # x #== policyId)
              ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                  pfromData $ pfstBuiltin # (phead # tokens)
              )
              (self # xs)
        )
        (const perror)
        # pto val'

-- | Finds amount of the first asset in a value that doesn't have ownPolicyId as its currency symbol.
ptryFindAmt ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> PValue keys amounts :--> PInteger)
ptryFindAmt = phoistAcyclic $
  plam $ \ownPolicyId val ->
    pmatch val $ \(PValue val') ->
      pmatch val' $ \(PMap csPairs) ->
        precList
          ( \self x xs ->
              pif
                (pnot # (pfromData (pfstBuiltin # x) #== ownPolicyId))
                ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                    pfoldr
                      # plam
                        ( \x acc ->
                            plet (pfromData $ psndBuiltin # x) $ \q ->
                              pif
                                (0 #< q)
                                (q + acc)
                                acc
                        )
                      # 0
                      # tokens
                )
                (self # xs)
          )
          (const $ ptraceError "ptryFindAmt")
          # csPairs

phasInput :: Term s (PBuiltinList PTxInInfo :--> PTxOutRef :--> PBool)
phasInput = phoistAcyclic $ plam $ \refs oref -> pany # plam (\oref' -> oref #== pfield @"outRef" # oref') # refs

pvalueContains ::
  ClosedTerm
    ( PValue 'Sorted 'Positive
        :--> PValue 'Sorted 'Positive
        :--> PBool
    )
pvalueContains = phoistAcyclic $
  plam $ \superset subset ->
    let forEachTN cs = plam $ \tnPair ->
          let tn = pfromData $ pfstBuiltin # tnPair
              amount = pfromData $ psndBuiltin # tnPair
           in amount #<= pvalueOf # superset # cs # tn
        forEachCS = plam $ \csPair ->
          let cs = pfromData $ pfstBuiltin # csPair
              tnMap = pto $ pfromData $ psndBuiltin # csPair
           in pall # forEachTN cs # tnMap
     in pall # forEachCS #$ pto $ pto subset

{- | Extract the token name and the amount of the given currency symbol.
Throws when the token name is not found or more than one token name is involved
Plutarch level function.
-}
ponlyAsset ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PTriple PCurrencySymbol PTokenName PInteger)
ponlyAsset = phoistAcyclic $
  plam $ \val ->
    pmatch val $ \(PValue val') ->
      plet (pheadSingleton # pto val') $ \valuePair ->
        pmatch (pfromData (psndBuiltin # valuePair)) $ \(PMap tokens) ->
          plet (pheadSingleton # tokens) $ \tkPair ->
            pcon (PTriple (pfromData (pfstBuiltin # valuePair)) (pfromData (pfstBuiltin # tkPair)) (pfromData (psndBuiltin # tkPair)))

{- | Expand the given list of conditions with pand'.
Evaluates the arguments strictly.
-}
pand'List :: [Term s PBool] -> Term s PBool
pand'List = foldr1 (\res x -> pand' # res # x)

{- | Build nested conditions. It takes an association list of conditions and
and results. It evaluates the conditions in order: whenever a condition
is satisfied, its associated result is returned.
Expands to a nested `pif` structure.
-}
pcond ::
  [(Term s PBool, Term s a)] ->
  Term s a ->
  Term s a
pcond [] def = def
pcond ((cond, x) : conds) def = pif cond x $ pcond conds def

(#>) :: POrd t => Term s t -> Term s t -> Term s PBool
a #> b = b #< a

infix 4 #>

(#>=) :: (POrd t) => Term s t -> Term s t -> Term s PBool
a #>= b = b #<= a

infix 4 #>=

(#/=) :: PEq t => Term s t -> Term s t -> Term s PBool
a #/= b = pnot # (a #== b)

infix 4 #/=
