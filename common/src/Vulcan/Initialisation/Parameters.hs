{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Vulcan.Initialisation.Parameters (marketORef) where

import Ledger.Exports.V2 (TxOutRef (TxOutRef))

-- TODO: Update to actual terms and ref.

marketORef :: TxOutRef
marketORef = TxOutRef "ecbbb91f1817c045e8931952f93411b484c0a6b4cc4be8426d84f069531edc82" 0
