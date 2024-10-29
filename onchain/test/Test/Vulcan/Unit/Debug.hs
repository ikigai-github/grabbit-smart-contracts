{-# LANGUAGE ImpredicativeTypes #-}

{- |
  This module is for manual plutarch-terms debugging.
  Failing tests for plutarch terms lacks of onchain traces.
  To normally debug failed plutarch term you'd possibly need `showEvalResult`
  that shows evaluation result as well, as traces if evaluation succeed with `perror`.
-}
module Test.Vulcan.Unit.Debug (
  EvalResult,
  showEvalResult,
) where

import Data.Text (Text)
import Plutarch qualified
import Plutarch.Evaluate (evalTerm)
import Vulcan.Common.Types.Instances (Config (Config), TracingMode (DoTracing), Verbosity (Verbose))
import Vulcan.Utils (toPlutarchConfig)

data EvalResult
  = SucceedButPError [Text]
  | Succed
  | Failed
  deriving stock (Show)

showEvalResult :: forall {a :: PType}. ClosedTerm a -> EvalResult
showEvalResult testCase = case evalTerm verboseCfg testCase of
  Right (Right _, _, _) -> Succed
  Right (_, _, traces) -> SucceedButPError traces
  _ -> Failed

verboseCfg :: Plutarch.Config
verboseCfg = toPlutarchConfig $ Config $ DoTracing Verbose
