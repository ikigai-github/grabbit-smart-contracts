{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vulcan.Onchain.Metadata.Types where

import Ledger.Exports.V1 (Value (..))
import Plutarch
import Plutarch.Api.V2 (AmountGuarantees (Positive), KeyGuarantees (Sorted), PAddress, PMap, PValue)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import PlutusLedgerApi.V1 (Address, BuiltinByteString, TxOutRef, CurrencySymbol)
import PlutusTx (BuiltinData)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import Plutarch.Api.V1 (PCurrencySymbol)
import Plutarch.Api.V1 (PTxOutRef)

data DatumMetadata = DatumMetadata
  { metadata :: AssocMap.Map BuiltinByteString BuiltinByteString
  , version :: Integer
  , extra :: BuiltinData
  }
PlutusTx.makeLift ''DatumMetadata
PlutusTx.makeIsDataIndexed ''DatumMetadata [('DatumMetadata, 0)]

data PMetadataDatum (s :: S)
  = PMetadataDatum
      ( Term
          s
          ( PDataRecord
              '[ "metadata" ':= PMap 'Sorted PByteString PByteString
               , "version" ':= PInteger
               , "extra" ':= PData
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PMetadataDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PMetadataDatum where
  type PLifted PMetadataDatum = DatumMetadata

deriving via
  (DerivePConstantViaData DatumMetadata PMetadataDatum)
  instance
    (PConstantDecl DatumMetadata)

instance PTryFrom PData PMetadataDatum
instance PTryFrom PData (PAsData PMetadataDatum)

data EvolutionInfo = EvolutionInfo
  { stage :: Integer
  , stages :: Integer
  , price :: Value
  , recipient :: Address
  , isPaid :: Bool
  }

PlutusTx.makeLift ''EvolutionInfo
PlutusTx.makeIsDataIndexed ''EvolutionInfo [('EvolutionInfo, 0)]

instance PTryFrom PData (PAsData PBool)

data PEvolutionInfo (s :: S)
  = PEvolutionInfo
      ( Term
          s
          ( PDataRecord
              '[ "stage" ':= PInteger
               , "stages" ':= PInteger
               , "price" ':= PValue 'Sorted 'Positive
               , "recipient" ':= PAddress
               , "isPaid" ':= PBool
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PEvolutionInfo where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PEvolutionInfo where
  type PLifted PEvolutionInfo = EvolutionInfo

deriving via
  (DerivePConstantViaData EvolutionInfo PEvolutionInfo)
  instance
    (PConstantDecl EvolutionInfo)

instance PTryFrom PData PEvolutionInfo
instance PTryFrom PData (PAsData PEvolutionInfo)

data MetadataEvolveDatum = MetadataEvolveDatum {metadata :: BuiltinData, version :: Integer, extra :: EvolutionInfo}

PlutusTx.makeLift ''MetadataEvolveDatum
PlutusTx.makeIsDataIndexed ''MetadataEvolveDatum [('MetadataEvolveDatum, 0)]

data PMetadataEvolveDatum (s :: S)
  = PMetadataEvolveDatum
      ( Term
          s
          ( PDataRecord
              '[ "metadata" ':= PData
               , "version" ':= PInteger
               , "extra" ':= PEvolutionInfo
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PMetadataEvolveDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PMetadataEvolveDatum where
  type PLifted PMetadataEvolveDatum = MetadataEvolveDatum

deriving via
  (DerivePConstantViaData MetadataEvolveDatum PMetadataEvolveDatum)
  instance
    (PConstantDecl MetadataEvolveDatum)

instance PTryFrom PData PMetadataEvolveDatum
instance PTryFrom PData (PAsData PMetadataEvolveDatum)

data NFTMintConfig = NFTMintConfig
  { initUTxO :: TxOutRef
  , threadToken :: CurrencySymbol 
  , marketplaceFee :: Value
  , marketplaceAddress :: Address
  , projectFee :: Value
  , projectAddress :: Address 
  }

PlutusTx.makeLift ''NFTMintConfig
PlutusTx.makeIsDataIndexed ''NFTMintConfig [('NFTMintConfig, 0)]

data PNFTMintConfig (s :: S)
  = PNFTMintConfig
      ( Term
          s
          ( PDataRecord
              '[ "initUTxO" ':= PTxOutRef 
               , "threadToken" ':= PCurrencySymbol
               , "marketplaceFee" ':= PValue 'Sorted 'Positive
               , "marketplaceAddress" ':= PAddress
               , "projectFee" ':= PValue 'Sorted 'Positive
               , "projectAddress" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PNFTMintConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PNFTMintConfig where
  type PLifted PNFTMintConfig = NFTMintConfig

deriving via
  (DerivePConstantViaData NFTMintConfig PNFTMintConfig)
  instance
    (PConstantDecl NFTMintConfig)

instance PTryFrom PData PNFTMintConfig
instance PTryFrom PData (PAsData PNFTMintConfig)

data ThreadConfig = ThreadConfig
  { initUTxO :: TxOutRef
  , lanes :: Integer
  , count :: Integer
  , refundTo :: Address  
  }

PlutusTx.makeLift ''ThreadConfig
PlutusTx.makeIsDataIndexed ''ThreadConfig [('ThreadConfig, 0)]

data PThreadConfig (s :: S)
  = PThreadConfig
      ( Term
          s
          ( PDataRecord
              '[ "initUTxO" ':= PTxOutRef 
               , "lanes" ':= PInteger
               , "count" ':= PInteger 
               , "threadScript" ':= PAddress 
               , "refundTo" ':= PAddress 
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PThreadConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PThreadConfig where
  type PLifted PThreadConfig = ThreadConfig

deriving via
  (DerivePConstantViaData ThreadConfig PThreadConfig)
  instance
    (PConstantDecl ThreadConfig)

instance PTryFrom PData PThreadConfig
instance PTryFrom PData (PAsData PThreadConfig)