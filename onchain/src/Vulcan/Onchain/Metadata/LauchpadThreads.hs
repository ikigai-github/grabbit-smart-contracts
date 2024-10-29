{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Vulcan.Onchain.Metadata.NFTLaunchpad where

import GHC.Stack (HasCallStack)
import Plutarch (Config (Config), TracingMode (DoTracing))
import Plutarch.Api.V1 (PCredential (..), PValidatorHash)
import Plutarch.Api.V1.Value (padaToken, pnoAdaValue, pnormalize, pvalueOf)
import Plutarch.Api.V2 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PAddress (..),
  PCurrencySymbol,
  PMintingPolicy,
  POutputDatum (POutputDatum),
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PMinting, PSpending),
  PTokenName (..),
  PTxOut,
  PTxOutRef,
  PValidator,
  PValue (..),
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
 )
import Plutarch.Extra.ScriptContext (pfromPDatum)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
  ptraceC,
  ptryFromC,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Num ((#+))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (
  Address (..),
  CurrencySymbol,
  MintingPolicy,
  PubKeyHash,
  TxOutRef,
  ValidatorHash,
  Value (..),
 )
import PlutusTx qualified
import Vulcan.Onchain.Collections.Utils (
  PTriple (PTriple),
  pand'List,
  pcond,
  phasInput,
  ponlyAsset,
  ptryLookupValue,
  ptryOutputToAddress,
  ptryOwnInput,
  ptryOwnOutput,
  (#>),
 )
import Vulcan.Onchain.Metadata.Constants
import Vulcan.Onchain.Metadata.Types (PMetadataDatum (..), PMetadataEvolveDatum(..), PThreadConfig(..))

data ThreadMintAction
  = MintStateThread
  | BurnStateThread
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''ThreadMintAction

data PThreadMintAction (s :: S)
  = PMintThread (Term s (PDataRecord '[]))
  | PBurnThread (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PThreadMintAction where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PThreadMintAction where
  type PLifted PThreadMintAction = ThreadMintAction

deriving via
  (DerivePConstantViaData ThreadMintAction PThreadMintAction)
  instance
    (PConstantDecl ThreadMintAction)

instance PTryFrom PData PThreadMintAction
instance PTryFrom PData (PAsData PThreadMintAction)

data ThreadDatum = ThreadDatum {counter :: Integer, laneId :: Integer}
  deriving stock (Generic, Show)
PlutusTx.unstableMakeIsData ''ThreadDatum
 
data PThreadDatum (s :: S)
  = PThreadDatum
      ( Term
          s
          ( PDataRecord
              '[ "counter" ':= PInteger,  
                 "laneId" ':= PInteger,
                 "refundTo" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PThreadDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PThreadDatum where
  type PLifted PThreadDatum = ThreadDatum

deriving via
  (DerivePConstantViaData ThreadDatum PThreadDatum)
  instance
    (PConstantDecl ThreadDatum)

instance PTryFrom PData PThreadDatum

instance PTryFrom PData (PAsData PThreadDatum)

pcheckThreadMint :: Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool)
pcheckThreadMint = phoistAcyclic $
  plam $ \tkPairs ->
    ( pfix #$ plam $ \self i xs ->
        pelimList
          ( \tokPair tokPairs ->
              let checkName = 
                    plet (pto tn) $ \tnBS ->
                      pand'List 
                        [ (plengthBS # tnBS) #== 6
                        , laneTN #== (psliceBS # 0 # i # tnBS) 
                        , i #== pindexBS # tnBS # 5
                        ]
              pif (pand' # (psndBuiltin # tokPair #== 1) # checkName) (self # i + 1 # tokPairs) (pconstant False)
          )
          (pconstant True)
          xs
    )
      # 0
      # tkPairs

pthreadStateMint :: Term s (PThreadConfig :--> PThreadMintAction :--> PScriptContext :--> PUnit)
pthreadStateMint = phoistAcyclic $
  plam $ \threadConfig redm context -> unTermCont $ do
    threadConfigF <- pletFieldsC @'["initUTxO", "lanes", "count", "threadScript", "refundTo"] threadConfig 
    contextFields <- pletFieldsC @'["txInfo", "purpose"] context
    PMinting policy <- pmatchC contextFields.purpose
    ownPolicyId <- pletC $ pfield @"_0" # policy
    txInfoFields <- pletFieldsC @'["inputs", "outputs", "mint"] contextFields.txInfo
    mintedValue <- pletC $ pnormalize # txInfoFields.mint
    let isUtxoSpent = phasInput # txInfoFields.inputs # threadConfigF.initUTxO
    tkPairs <- pletC $ ptryLookupValue # ownPolicyId # txInfoFields.mint
    laneTokens <- pletC (pheadSingleton # tkPairs)
    txOutputs <- pletC txInfoFields.outputs

    pure $
      pif
        ( pmatch
            redm
            ( \case
                PMintStateThread _ ->
                 pand'List 
                  [ isUTxOSpent 
                  , threadConfigF.lanes #== psndBuiltin # laneTokens 
                  , pfstBuiltin # laneTokens #== laneTN 
                  , pcheckThreadOutputs 
                      # txOutputs 
                      # pfromData ownPolicyId 
                      # threadConfigF.threadScript 
                      # threadConfigF.refundTo 
                      # threadConfigF.count 
                  ]
                PBurnStateThread _ ->
                  (psndBuiltin # laneTokens) #< 0
            )
        )
        (pconstant ())
        perror
  where
    pcheckThreadOutputs :: 
      PIsListLike list PTxOut => 
      Term s (list PTxOut) ->
      Term s PCurrencySymbol 
      Term s PAddress -> 
      Term s PAddress ->
      Term s PInteger
      Term s PBool
    pcheckThreadOutputs outs ownCS threadScript refundTo count = 
        ( pfix #$ plam $ \self i xs ->
            pelimList
              ( \txo txos -> pletFields @'["address", "value", "datum"] txo $ \txoFields ->
                  plet (pvalueOf # txoFields.value # ownCS # laneTN) $ \numMinted -> 
                    pif (numMinted #> 0)
                        ( pand #
                          ( pmatch
                              txoFields.datum
                              ( \case
                                  POutputDatum scriptOutputD ->
                                    pletFields @'["counter", "laneId", "refundTo"]
                                      (pfromPDatum @PThreadDatum # (pfield @"outputDatum" # scriptOutputD))
                                      ( \datumFields ->
                                          pand'List 
                                            [ pfromData datumFields.counter #== count
                                            , pfromData datumFields.laneId #== i 
                                            , pfromData datumFields.refundTo #== refundTo 
                                            , numMinted #== pconstant 1
                                            . txoFields.address #== threadScript 
                                            ]
                                      )
                                  _ -> pconstant False
                              )
                          ) #
                          (self # i + 1 # txos)
                        )
                        (self # i # txos)
              )
              (pconstant True)
              xs
        ) # outs

pthreadStateMintingPolicy :: Term s (PThreadConfig :--> PMintingPolicy)
pthreadStateMintingPolicy = phoistAcyclic $ plam $ \launchConfig redm ctx -> 
  let redeemer = ptryFrom @PThreadMintAction redm fst in popaque $ pthreadStateMint # launchConfig # redeemer # ctx

data ThreadAction 
  = ContinueThread
  | TerminateThread
  deriving stock (Generic, Show)
PlutusTx.unstableMakeIsData ''ThreadAction

data PThreadAction (s :: S)
  = PContinueThread (Term s (PDataRecord '[]))
  | PTerminateThread (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PThreadAction where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PThreadAction where
  type PLifted PThreadAction = ThreadAction

deriving via
  (DerivePConstantViaData ThreadAction PThreadAction)
  instance
    (PConstantDecl ThreadAction)

pthreadValidator :: Term s (PThreadDatum :--> PThreadAction :--> PScriptContext :--> PUnit)
pthreadValidator = phoistAcyclic $
  plam $ \threadDatum threadAct ctx -> unTermCont $ do
    contextFields <- pletFieldsC @'["txInfo", "purpose"] ctx
    PSpending ownRef' <- pmatchC contextFields.purpose
    ownRef <- pletC $ pfield @"_0" # ownRef'

    txInfoFields <- pletFieldsC @'["inputs", "outputs", "signatories", "mint"] contextFields.txInfo

    let ownInput = ptryOwnInput # txInfoFields.inputs # ownRef
    ownInputFields <- pletFieldsC @'["address", "value"] ownInput
    ownInputValue <- plet ownInputFields.value 
    ownValueNoAda <- pletC $ pnoAdaValue # ownInputValue 

    oldDatumFields <- pletFields @'["counter", "laneId", "refundTo"] threadDatum

    PScriptCredential ownInputScriptCredential <- pmatchC (pfield @"credential" # ownInputFields.address)
    ownValHash <- pletC (pfield @"_0" # ownInputScriptCredential)

    pure $
      pmatch
        threadAct
        ( \case
            PContinueThread _ ->
              let ownOutput = ptryOwnOutput # txInfoFields.outputs # ownValHash
               in pletFields @'["value", "datum"] ownOutput $ \ownOutputFields ->
                    pmatch ownOutputFields.datum ( \case
                      (POutputDatum newThreadDat) ->
                        let newThreadDatum = pfromPDatum @PThreadDatum # (pfield @"outputDatum" # newThreadDat)
                         in pletFields @'["counter", "laneId", "refundTo"]
                              newThreadDatum
                              ( \newDatumFields ->
                                  pif
                                    ( pand'List 
                                        [ newThreadDatum.counter #== oldDatumFields.counter - 1
                                        , newThreadDatum.laneId #== oldDatumFields.laneId 
                                        , newThreadDatum.refundTo #== oldDatumFields.refundTo
                                        , ownValueNoAda #== pnoAdaValue # ownOutputFields.value 
                                        ] 
                                    )
                                    (pconstant ())
                                    perror
                              )
                      _ -> perror
                    )
            PTerminateThread _ ->
              plet ( pvalueOf # ownInputValue # "" # "" ) $ \ownValueAda ->
                pmatch (ponlyAsset # ownValueNoAda) (\(PTriple laneCS laneTN _amt) -> 
                ( pif
                    (pany # plam (\txo -> pletFields @'["address", "value"] txo $ \ txoF -> 
                                    ( txoF.address #== oldDatumFields.refundTo )
                                      #&& ( pvalueOf # txoF.value # "" # "" #>= ownValueNoAda )
                          # txInfoFields.outputs)
                      #&& (pvalueOf # txInfoFields.mint # laneCS # laneTN) #== -1)
                      #&& oldDatumFields.counter #== 1
                    (pconstant ())
                    perror
                ))
        )

pthreadValidatorW :: Term s PValidator
pthreadValidatorW = phoistAcyclic $ plam $ \dat redm ctx ->
  popaque $ pthreadValidator # punsafeCoerce dat # punsafeCoerce redm # ctx
