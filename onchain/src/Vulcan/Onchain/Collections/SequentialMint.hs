{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Vulcan.Onchain.Collections.SequentialMint where

import GHC.Stack (HasCallStack)
import Plutarch (Config (Config), TracingMode (DoTracing))
import Plutarch.Api.V1 (PCredential (..), PValidatorHash)
import Plutarch.Api.V1.Value (pnormalize, pvalueOf)
import Plutarch.Api.V2 (
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
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Num ((#+))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  MintingPolicy,
  PubKeyHash,
  TxOutRef,
  ValidatorHash,
 )
import PlutusTx qualified
import Vulcan.Onchain.Collections.Utils

data SequentialMintAction
  = MintStateThread
  | BurnStateThread
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''SequentialMintAction

data PSequentialMintAction (s :: S)
  = PMintStateThread (Term s (PDataRecord '[]))
  | PBurnStateThread (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PSequentialMintAction where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PSequentialMintAction where
  type PLifted PSequentialMintAction = SequentialMintAction

deriving via
  (DerivePConstantViaData SequentialMintAction PSequentialMintAction)
  instance
    (PConstantDecl SequentialMintAction)

instance PTryFrom PData PSequentialMintAction
instance PTryFrom PData (PAsData PSequentialMintAction)

data SequenceDatum = SequenceDatum {mintCount :: Integer, threshold :: Integer}
  deriving stock (Generic, Show)
PlutusTx.unstableMakeIsData ''SequenceDatum

data PSequenceDatum (s :: S)
  = PSequenceDatum
      ( Term
          s
          ( PDataRecord
              '[ "mintCount" ':= PInteger
               , "threshold" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PSequenceDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PSequenceDatum where
  type PLifted PSequenceDatum = SequenceDatum

deriving via
  (DerivePConstantViaData SequenceDatum PSequenceDatum)
  instance
    (PConstantDecl SequenceDatum)

instance PTryFrom PData PSequenceDatum

instance PTryFrom PData (PAsData PSequenceDatum)

pvalidateSeqStateMint :: Term s (PTxOutRef :--> PSequentialMintAction :--> PScriptContext :--> PUnit)
pvalidateSeqStateMint = phoistAcyclic $
  plam $ \oref redm context -> unTermCont $ do
    contextFields <- tcont (pletFields @'["txInfo", "purpose"] context)

    PMinting policy <- pmatchC contextFields.purpose

    ownPolicyId <- pletC $ pfield @"_0" # policy

    txInfoFields <- tcont (pletFields @'["inputs", "outputs", "mint"] contextFields.txInfo)
    mintedValue <- pletC $ pnormalize # txInfoFields.mint
    mintedRTs <- pletC $ ppositiveSymbolValueOf # ownPolicyId # mintedValue
    burnedRTs <- pletC $ pnegativeSymbolValueOf # ownPolicyId # mintedValue

    let isUtxoSpent = phasInput # txInfoFields.inputs # oref
        mintedTn = pfirstTokenNameWithCS # pdata ownPolicyId # mintedValue

    -- Debug minted token name
    ptraceC (pshow mintedTn)

    txOutputs <- pletC txInfoFields.outputs

    pure $
      pif
        ( pmatch
            redm
            ( \case
                PMintStateThread _ ->
                  plet (pcountCS # mintedValue) $ \numCS ->
                    mintedRTs #== 1
                      #&& burnedRTs #== 0
                      #&& isUtxoSpent
                      #&& pif
                        (numCS #== 1)
                        (pany # (mintsToVH # mintedTn # ownPolicyId # 0) # txOutputs)
                        ( pif
                            (numCS #== 2)
                            -- numNFTMinted is the upper bound of the number of sequential NFTs minted in this tx.
                            ( let numNFTMinted = ptryFindAmt # ownPolicyId # mintedValue
                               in (pany # (mintsToVH # mintedTn # ownPolicyId # numNFTMinted) # txOutputs)
                            )
                            (pconstant False)
                        )
                PBurnStateThread _ ->
                  mintedRTs #== 0 #&& burnedRTs #== -1
            )
        )
        (pconstant ())
        perror
  where
    mintsToVH :: Term s (PTokenName :--> PCurrencySymbol :--> PInteger :--> PTxOut :--> PBool)
    mintsToVH =
      phoistAcyclic $
        plam
          ( \mintedTn ownPolicyId mcount txo -> pletFields @'["address", "value", "datum"] txo $ \txoFields ->
              pmatch (pfield @"credential" # txoFields.address) $ \case
                -- To debug the cvalHash use:
                -- ptrace (pshow (pto (pfromData (pfield @"_0" # cvalHash))))
                PScriptCredential cvalHash ->
                  pto (pfromData (pfield @"_0" # cvalHash)) #== pto mintedTn
                    #&& pvalueOf # txoFields.value # ownPolicyId # mintedTn #== 1
                    #&& pmatch
                      txoFields.datum
                      ( \case
                          POutputDatum scriptOutputD ->
                            pletFields @'["mintCount", "threshold"]
                              (pfromPDatum @PSequenceDatum # (pfield @"outputDatum" # scriptOutputD))
                              ( \datumFields ->
                                  pfromData datumFields.mintCount #== mcount #&& mcount #< pfromData datumFields.threshold
                              )
                          _ -> pconstant False
                      )
                PPubKeyCredential _ -> pconstant False
          )

pseqStateMintingPolicy :: Term s (PTxOutRef :--> PMintingPolicy)
pseqStateMintingPolicy = phoistAcyclic $ plam $ \oref redm ctx -> let redeemer = ptryFrom @PSequentialMintAction redm fst in popaque $ pvalidateSeqStateMint # oref # redeemer # ctx

defaultConfig :: Config
defaultConfig = Config DoTracing

-- | Converts a Plutarch minting script to Plutus script
toMintingPolicy :: HasCallStack => ClosedTerm PMintingPolicy -> MintingPolicy
toMintingPolicy = mkMintingPolicy defaultConfig

seqStateMintingPolicy :: TxOutRef -> MintingPolicy
seqStateMintingPolicy oref =
  toMintingPolicy (pseqStateMintingPolicy # pconstant oref)

seqStateTokenCurrencySymbol :: TxOutRef -> CurrencySymbol
seqStateTokenCurrencySymbol = mintingPolicySymbol . seqStateMintingPolicy

data SequenceRedeemer
  = MintSequenceNFT
  | EndMint
  deriving stock (Generic, Show)
PlutusTx.unstableMakeIsData ''SequenceRedeemer

data PSequenceRedeemer (s :: S)
  = PMintSequenceNFT (Term s (PDataRecord '[]))
  | PEndMint (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PSequenceRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PSequenceRedeemer where
  type PLifted PSequenceRedeemer = SequenceRedeemer

deriving via
  (DerivePConstantViaData SequenceRedeemer PSequenceRedeemer)
  instance
    (PConstantDecl SequenceRedeemer)

data SequenceParametersD = SequenceParametersD
  { seqStateCS :: CurrencySymbol
  , sequenceOwner :: PubKeyHash
  }
  deriving stock (Generic, Show)
PlutusTx.unstableMakeIsData ''SequenceParametersD

data PSequenceParametersD (s :: S)
  = PSequenceParametersD
      ( Term
          s
          ( PDataRecord
              '[ "seqStateCS" ':= PCurrencySymbol
               , "seqOwner" ':= PPubKeyHash
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PSequenceParametersD where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PSequenceParametersD where
  type PLifted PSequenceParametersD = SequenceParametersD

deriving via
  (DerivePConstantViaData SequenceParametersD PSequenceParametersD)
  instance
    (PConstantDecl SequenceParametersD)

pseqValidator :: Term s (PSequenceParametersD :--> PSequenceDatum :--> PSequenceRedeemer :--> PScriptContext :--> PUnit)
pseqValidator = phoistAcyclic $
  plam $ \pseqParams psequenceDatum psequenceRedeemer ctx -> unTermCont $ do
    pseqParamsF <- pletFieldsC @'["seqStateCS", "seqOwner"] pseqParams
    contextFields <- pletFieldsC @'["txInfo", "purpose"] ctx
    PSpending ownRef' <- pmatchC contextFields.purpose
    ownRef <- pletC $ pfield @"_0" # ownRef'

    txInfoFields <- pletFieldsC @'["inputs", "outputs", "mint", "signatories", "datums"] contextFields.txInfo

    mintedValue <- pletC $ pnormalize # txInfoFields.mint
    numMintedCS <- pletC (pcountCS # mintedValue)

    ownInput <- pletC $ ptryOwnInput # txInfoFields.inputs # ownRef
    ownInputFields <- pletFieldsC @'["address", "value"] ownInput
    PScriptCredential ownInputScriptCredential <- pmatchC (pfield @"credential" # ownInputFields.address)
    ownValHash <- pletC (pfield @"_0" # ownInputScriptCredential)
    ownOutputs <- pletC $ pfilter # (paysToCredential # ownValHash) # txInfoFields.outputs
    signedBySeqOwner <- pletC $ pelem # pseqParamsF.seqOwner # txInfoFields.signatories

    pure $
      pmatch
        psequenceRedeemer
        ( \case
            PMintSequenceNFT _ ->
              let ownOutput = pheadSingleton # ownOutputs
                  -- numNFTMinted is the upper bound of the number of sequential NFTs minted in this tx.
                  numNFTMinted = ptryFindAmt # pfromData pseqParamsF.seqStateCS # mintedValue
               in pletFields @'["value", "datum"] ownOutput $ \ownOutputFields ->
                    pmatch ownOutputFields.datum $ \case
                      (POutputDatum newSeqDat) ->
                        let newSequenceDatum = pfromPDatum @PSequenceDatum # (pfield @"outputDatum" # newSeqDat)
                         in pletFields @'["mintCount", "threshold"]
                              psequenceDatum
                              ( \oldDatumFields ->
                                  pletFields @'["mintCount", "threshold"]
                                    newSequenceDatum
                                    ( \newDatumFields ->
                                        plet (pfromData newDatumFields.mintCount) $ \newMintCount ->
                                          pif
                                            ( signedBySeqOwner
                                                #&& newMintCount #< pfromData oldDatumFields.threshold
                                                #&& ownOutputFields.value #== ownInputFields.value
                                                #&& newMintCount #== (oldDatumFields.mintCount #+ numNFTMinted)
                                                #&& oldDatumFields.threshold #== newDatumFields.threshold
                                                #&& pvalueOf # ownOutputFields.value # pfromData pseqParamsF.seqStateCS # pcon (PTokenName (pto ownValHash)) #== 1
                                                #&& numMintedCS #== 1
                                            )
                                            (pconstant ())
                                            perror
                                    )
                              )
                      _ -> perror
            PEndMint _ ->
              ( pif
                  ( pnull # ownOutputs
                      #&& pvalueOf # mintedValue # pfromData pseqParamsF.seqStateCS # pcon (PTokenName (pto ownValHash)) #== -1
                      #&& signedBySeqOwner
                  )
                  (pconstant ())
                  perror
              )
        )

pseqValidatorW :: Term s (PSequenceParametersD :--> PValidator)
pseqValidatorW = phoistAcyclic $ plam $ \seqParams dat redm ctx -> popaque $ pseqValidator # seqParams # punsafeCoerce dat # punsafeCoerce redm # ctx

data SequentialNFTRedeemer
  = MintSequentialNFT
  | BurnSequentialNFT
  | MintFirstNFT
  deriving stock (Generic, Show)
PlutusTx.unstableMakeIsData ''SequentialNFTRedeemer

data PSequentialNFTRedeemer (s :: S)
  = PMintSequentialNFT (Term s (PDataRecord '[]))
  | PBurnSequentialNFT (Term s (PDataRecord '[]))
  | PMintFirstNFT (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PSequentialNFTRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PSequentialNFTRedeemer where
  type PLifted PSequentialNFTRedeemer = SequentialNFTRedeemer

deriving via
  (DerivePConstantViaData SequentialNFTRedeemer PSequentialNFTRedeemer)
  instance
    (PConstantDecl SequentialNFTRedeemer)

psequentialNFTMint :: Term s (PValidatorHash :--> PCurrencySymbol :--> PSequentialNFTRedeemer :--> PScriptContext :--> PUnit)
psequentialNFTMint = phoistAcyclic $
  plam $ \pseqValHash pseqStateCS psequentialRedeemer ctx -> unTermCont $ do
    contextFields <- tcont (pletFields @'["txInfo", "purpose"] ctx)

    PMinting policy <- pmatchC contextFields.purpose
    let ownPolicyId = pfield @"_0" # policy

    txInfoFields <- tcont (pletFields @'["inputs", "outputs", "mint"] contextFields.txInfo)
    mintedValue <- pletC $ txInfoFields.mint
    mintedRTs <- pletC $ ppositiveSymbolValueOf # ownPolicyId # mintedValue
    burnedRTs <- pletC $ pnegativeSymbolValueOf # ownPolicyId # mintedValue

    pure $
      pif
        ( pmatch
            psequentialRedeemer
            ( \case
                PMintSequentialNFT _ ->
                  mintedRTs #> 0 #&& burnedRTs #== 0
                    #&& ( pany
                            # plam
                              ( \txinp -> pletFields @'["resolved"] txinp $ \txInFields ->
                                  paysToCredential # pseqValHash # txInFields.resolved
                              )
                            # pfromData txInfoFields.inputs
                        )
                PBurnSequentialNFT _ ->
                  (mintedRTs #== 0 #&& burnedRTs #== -1)
                PMintFirstNFT _ ->
                  mintedRTs #> 0
                    #&& burnedRTs #== 0
                    #&& pvalueOf # mintedValue # pseqStateCS # pcon (PTokenName (pto pseqValHash)) #== 1
            )
        )
        (pconstant ())
        perror

psequentialNFTMintW :: Term s (PAsData PValidatorHash :--> PAsData PCurrencySymbol :--> PMintingPolicy)
psequentialNFTMintW = phoistAcyclic $ plam $ \pseqValHash pseqStateCS redm ctx -> popaque $ psequentialNFTMint # pfromData pseqValHash # pfromData pseqStateCS # punsafeCoerce redm # ctx

sequentialNFTMintingPolicy :: ValidatorHash -> CurrencySymbol -> MintingPolicy
sequentialNFTMintingPolicy seqValHash seqStateCS =
  toMintingPolicy (psequentialNFTMintW # pdata (pconstant seqValHash) # pdata (pconstant seqStateCS))

sequentialNFTCurrencySymbol :: ValidatorHash -> CurrencySymbol -> CurrencySymbol
sequentialNFTCurrencySymbol seqValHash seqStateCS = mintingPolicySymbol (sequentialNFTMintingPolicy seqValHash seqStateCS)
