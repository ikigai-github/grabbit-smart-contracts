module Vulcan.Common.Utils (findWith, satisfies, unNatural, adaValue, adaToLovelace, amountOflovelace) where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)

import Ledger.Exports.V1 qualified as Ledger

import Vulcan.Common.Types.Auction (Positive)

adaToLovelace :: Integer -> Integer
adaToLovelace = (*) 1_000_000

adaValue :: Integer -> Ledger.Value
adaValue = Ledger.singleton Ledger.adaSymbol Ledger.adaToken

amountOflovelace :: Ledger.Value -> Integer
amountOflovelace =
  fromMaybe 0
    . findWith
      (\(cs, tn, amt) -> if isAda cs tn then Just amt else Nothing)
    . Ledger.flattenValue
  where
    isAda cs tn = cs == Ledger.adaSymbol && tn == Ledger.adaToken

unNatural :: forall (n :: Type). Num n => Positive -> n
unNatural = fromIntegral @Integer . unNatural

findWith :: (a -> Maybe b) -> [a] -> Maybe b
findWith f (a : as) = case f a of
  Nothing -> findWith f as
  mb -> mb
findWith _ [] = Nothing

satisfies :: forall (a :: Type). (a -> Bool) -> a -> Maybe a
satisfies p x
  | p x = Just x
  | otherwise = Nothing
