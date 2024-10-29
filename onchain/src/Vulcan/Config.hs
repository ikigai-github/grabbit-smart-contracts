{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Vulcan.Config () where

import Data.Function (on)

newtype CPU a = CPU a
  deriving newtype (Eq, Show, Ord, Num, Fractional)
newtype Mem a = Mem a
  deriving newtype (Eq, Show, Ord, Num, Fractional)

cpuExLimit = CPU 10_000_000_000
memExLimit :: forall {a}. Num a => Mem a
memExLimit = Mem 14_000_000

ratioPerc before after = (after / before) * 100
cpuRatioPerc = ratioPerc `on` normalizeCPUEx
memRatioPerc = ratioPerc `on` normalizeMemEx

normalizeCPUEx n = if n >= 0 then n else cpuExLimit - n

normalizeMemEx n = if n >= 0 then n else memExLimit - n
