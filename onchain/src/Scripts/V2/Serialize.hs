module Scripts.V2.Serialize (
  asCborHex,
  asCompiledMP,
  asCompiledValidator,
  asCompiled,
  scriptSize,
) where

import Cardano.Binary qualified as CBOR
import Codec.Serialise (serialise)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Plutarch.Api.V2 (mintingPolicySymbol, validatorHash)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  MintingPolicy (getMintingPolicy),
  Script,
  Validator (getValidator),
  ValidatorHash,
 )
import Vulcan.Common.Types.Instances (
  CompiledScript (MkCompiledScript, hash, scriptCborHex, version),
 )

scriptSize :: Script -> Integer
scriptSize = fromIntegral . SBS.length . serialiseScriptShort

serialiseScriptShort :: Script -> SBS.ShortByteString
serialiseScriptShort = SBS.toShort . LBS.toStrict . serialise

asCompiled ::
  Bifunctor f =>
  f Validator MintingPolicy ->
  f (CompiledScript ValidatorHash) (CompiledScript CurrencySymbol)
asCompiled = bimap asCompiledValidator asCompiledMP

asCompiledMP :: MintingPolicy -> CompiledScript CurrencySymbol
asCompiledMP mp =
  MkCompiledScript
    { scriptCborHex = asCborHex $ getMintingPolicy mp
    , version = "PlutusScriptV2"
    , hash = mintingPolicySymbol mp
    }

asCompiledValidator :: Validator -> CompiledScript ValidatorHash
asCompiledValidator val =
  MkCompiledScript
    { scriptCborHex = asCborHex $ getValidator val
    , version = "PlutusScriptV2"
    , hash = validatorHash val
    }

asRawCbor :: Script -> BS.ByteString
asRawCbor = CBOR.serialize' . SBS.toShort . LBS.toStrict . serialise

asCborHex :: Script -> T.Text
asCborHex = Text.decodeUtf8 . Base16.encode . asRawCbor
