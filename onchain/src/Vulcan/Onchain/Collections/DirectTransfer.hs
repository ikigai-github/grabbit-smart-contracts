{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Vulcan.Onchain.Collections.DirectTransfer (pdirectOfferValidatorW) where

import Plutarch.Api.V1 (PCredential (..))
import Plutarch.Api.V2 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PAddress,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PSpending),
  PValidator,
  PValue,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
 )

import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx qualified
import Vulcan.Onchain.Collections.Utils

data PDirectOfferDatum (s :: S)
  = PDirectOfferDatum
      ( Term
          s
          ( PDataRecord
              '[ "creator" ':= PAddress
               , "toBuy" ':= PValue 'Sorted 'Positive
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PDirectOfferDatum where
  type DPTStrat _ = PlutusTypeData

data DirectOfferRedeemer
  = Accept
  | Update
  | Cancel
  deriving stock (Generic, Show)
PlutusTx.unstableMakeIsData ''DirectOfferRedeemer

data PDirectOfferRedeemer (s :: S)
  = PAccept (Term s (PDataRecord '[]))
  | PUpdate (Term s (PDataRecord '[]))
  | PCancel (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PDirectOfferRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PDirectOfferRedeemer where
  type PLifted PDirectOfferRedeemer = DirectOfferRedeemer

deriving via
  (DerivePConstantViaData DirectOfferRedeemer PDirectOfferRedeemer)
  instance
    (PConstantDecl DirectOfferRedeemer)

-- Parameterized by the offeree (the user receiving this offer)
-- All offers received by a user can by found by querying the smart contract address obtained by applying the user's
-- PKH to the script.
pdirectOfferValidator :: Term s (PAsData PPubKeyHash :--> PDirectOfferDatum :--> PDirectOfferRedeemer :--> PScriptContext :--> PUnit)
pdirectOfferValidator = phoistAcyclic $
  plam $ \offeree dat redeemer ctx -> unTermCont $ do
    datF <- pletFieldsC @'["creator", "toBuy"] dat
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    infoF <- pletFieldsC @'["inputs", "outputs", "signatories"] ctxF.txInfo
    PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
    let ownInput = ptryOwnInput # infoF.inputs # ownRef
    ownInputF <- pletFieldsC @'["address", "value"] ownInput
    PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC (pfield @"credential" # ownInputF.address)
    txOuts <- pletC infoF.outputs
    ownOutputs <- pletC $ pfilter # (paysToCredential # ownValHash) # txOuts
    noContOutputs <- pletC $ pnull # ownOutputs

    pure $
      pif
        ( pmatch
            redeemer
            ( \case
                PAccept _ ->
                  plet (pheadSingleton #$ pfilter # (paysToAddress # datF.creator) # txOuts) $ \creatorOutput ->
                    plet (pfield @"value" # creatorOutput) $ \creatorOutputVal ->
                      pvalueContains # creatorOutputVal # datF.toBuy
                        #&& ptxSignedByPkh # offeree # infoF.signatories
                        #&& noContOutputs
                PUpdate _ ->
                  pmatch (pfield @"credential" # datF.creator) $ \case
                    PScriptCredential _ -> pconstant False
                    PPubKeyCredential ((pfield @"_0" #) -> creatorPKH) ->
                      ptxSignedByPkh # creatorPKH # infoF.signatories
                PCancel _ ->
                  let creatorOutput = (pheadSingleton #$ pfilter # (paysToAddress # datF.creator) # txOuts)
                   in noContOutputs
                        #&& ( pvalueContains # (pfield @"value" # creatorOutput) # ownInputF.value
                                #|| pmatch
                                  (pfield @"credential" # datF.creator)
                                  ( \case
                                      PScriptCredential _ -> pconstant False
                                      PPubKeyCredential ((pfield @"_0" #) -> creatorPKH) ->
                                        ptxSignedByPkh # creatorPKH # infoF.signatories
                                  )
                            )
            )
        )
        (pconstant ())
        perror

pdirectOfferValidatorW :: Term s (PAsData PPubKeyHash :--> PValidator)
pdirectOfferValidatorW = phoistAcyclic $
  plam $ \offeree dat redeemer ctx ->
    popaque $ pdirectOfferValidator # offeree # punsafeCoerce dat # punsafeCoerce redeemer # ctx
