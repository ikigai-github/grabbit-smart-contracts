module PlutarchEval.Test where

import Control.Monad.IO.Class
import Data.Text (
  Text,
  pack,
  unpack,
 )

import Plutarch
import Plutarch.Api.V2
import Plutarch.Builtin
import Plutarch.Evaluate
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Unsafe
import PlutusLedgerApi.V1.Scripts qualified as Scripts
import PlutusLedgerApi.V2
import PlutusPrelude (showText)
import PlutusTx qualified
import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Util (reflow)

import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise, encode)
import Data.Bifunctor (first)
import Data.ByteString.Base16 qualified as Base16
import Data.Default (def)
import Data.Text.Encoding qualified as TE
import PlutusLedgerApi.V1.Scripts (Script (unScript), applyArguments)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)

evalSerialize :: ClosedTerm a -> Either Text Text
evalSerialize x = encodeSerialise . (\(a, _, _) -> a) <$> evalT x
  where
    encodeSerialise :: Serialise a => a -> Text
    encodeSerialise = TE.decodeUtf8 . Base16.encode . Write.toStrictByteString . encode

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile (Config DoTracing) x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

evalWithArgsT' :: ClosedTerm a -> [Data] -> Either Text (Program DeBruijn DefaultUni DefaultFun (), ExBudget, [Text])
evalWithArgsT' x args =
  (\(res, budg, trcs) -> (unScript res, budg, trcs))
    <$> evalWithArgsT x args
