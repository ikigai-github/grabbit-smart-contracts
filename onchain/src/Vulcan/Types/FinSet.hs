{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vulcan.Types.FinSet (
  PNodeAction (..),
  PSepNodeAction (..),
  PSeparatorConfig (..),
  PSetNode (..),
  PNodeKey (..),
  isEmptySet,
  asPredecessorOf,
  asSuccessorOf,
  getNextPK,
  getCurrentPK,
  isFirstNode,
  isLastNode,
  mkNode,
  isNothing,
  validNode,
  mkBSNode,
) where

import Plutarch.Api.V2 (
  PAddress,
  PPOSIXTime,
  PPubKeyHash (PPubKeyHash),
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Monadic qualified as P

import Vulcan.Common.Types.FinSet (NodeAction, NodeKey, SepNodeAction, SeparatorConfig, SetNode)

import Ply.Plutarch (PlyArgOf)

import Vulcan.Utils (pfield0)

-- For details see specification documents and Vulcan.Common.Types module(s).

data PNodeKey (s :: S)
  = PKey (Term s (PDataRecord '["_0" ':= PByteString]))
  | PEmpty (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

deriving via
  (DerivePConstantViaData NodeKey PNodeKey)
  instance
    PConstantDecl NodeKey

instance PUnsafeLiftDecl PNodeKey where
  type PLifted PNodeKey = NodeKey

deriving anyclass instance
  PTryFrom PData PNodeKey

instance DerivePlutusType PNodeKey where type DPTStrat _ = PlutusTypeData

newtype PSetNode (s :: S)
  = PSetNode
      ( Term
          s
          ( PDataRecord
              '[ "key" ':= PNodeKey
               , "next" ':= PNodeKey
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PSetNode where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PSetNode

deriving anyclass instance
  PTryFrom PData (PAsData PSetNode)

instance PUnsafeLiftDecl PSetNode where
  type PLifted PSetNode = SetNode

deriving via
  (DerivePConstantViaData SetNode PSetNode)
  instance
    PConstantDecl SetNode

data PSeparatorConfig (s :: S)
  = PSeparatorConfig
      ( Term
          s
          ( PDataRecord
              '[ "signer" ':= PPubKeyHash
               , "cutOff" ':= PPOSIXTime
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PSeparatorConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PSeparatorConfig where
  type PLifted PSeparatorConfig = SeparatorConfig
deriving via
  (DerivePConstantViaData SeparatorConfig PSeparatorConfig)
  instance
    PConstantDecl SeparatorConfig

type instance PlyArgOf PSeparatorConfig = SeparatorConfig

mkNode :: Term s (PNodeKey :--> PNodeKey :--> PSetNode)
mkNode = phoistAcyclic $
  plam $ \key next ->
    pcon $
      PSetNode $
        pdcons @"key" # pdata key
          #$ pdcons @"next" # pdata next
          #$ pdnil

data PNodeAction (s :: S)
  = PInit (Term s (PDataRecord '[]))
  | PDeinit (Term s (PDataRecord '[]))
  | PInsert (Term s (PDataRecord '["keyToInsert" ':= PPubKeyHash, "coveringNode" ':= PSetNode]))
  | PRemove (Term s (PDataRecord '["keyToRemove" ':= PPubKeyHash, "coveringNode" ':= PSetNode]))
  | PRemoveAndDeinit (Term s (PDataRecord '["finalKey" ':= PPubKeyHash]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PNodeAction where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PNodeAction)

instance PUnsafeLiftDecl PNodeAction where
  type PLifted PNodeAction = NodeAction
deriving via
  (DerivePConstantViaData NodeAction PNodeAction)
  instance
    PConstantDecl NodeAction

data PSepNodeAction (s :: S)
  = PSepInit (Term s (PDataRecord '[]))
  | PSepDeinit (Term s (PDataRecord '[]))
  | PSepInsert (Term s (PDataRecord '["keyToInsert" ':= PPubKeyHash, "coveringNode" ':= PSetNode]))
  | PSepRemove (Term s (PDataRecord '["keyToRemove" ':= PPubKeyHash, "coveringNode" ':= PSetNode]))
  | PSepRemoveAndDeinit (Term s (PDataRecord '["finalKey" ':= PPubKeyHash]))
  | -- | separators must be sorted or validation will fail
    PInsertSeps (Term s (PDataRecord '["separators" ':= PBuiltinList (PAsData PByteString), "coveringNode" ':= PSetNode]))
  | PRemoveSeps (Term s (PDataRecord '["separators" ':= PBuiltinList (PAsData PByteString), "coveringNode" ':= PSetNode]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PSepNodeAction where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PSepNodeAction)

instance PUnsafeLiftDecl PSepNodeAction where
  type PLifted PSepNodeAction = SepNodeAction
deriving via
  (DerivePConstantViaData SepNodeAction PSepNodeAction)
  instance
    PConstantDecl SepNodeAction

-----------------------------------------------
-- Helpers:

mkBSNode :: ClosedTerm (PByteString :--> PByteString :--> PAsData PSetNode)
mkBSNode = phoistAcyclic $
  plam $ \key' next' ->
    let key = pcon $ PKey $ pdcons @"_0" # pdata key' #$ pdnil
        next = pcon $ PKey $ pdcons @"_0" # pdata next' #$ pdnil
     in pdata $ mkNode # key # next

-- | Checks that the node is the empty head node and the datum is empty
isEmptySet :: ClosedTerm (PAsData PSetNode :--> PBool)
isEmptySet = phoistAcyclic $
  plam $ \head -> P.do
    keys <- pletFields @'["key", "next"] head
    isNothing # pfromData keys.key #&& isNothing # pfromData keys.next

-- | Checks that a PubKeyHash does belong to the first Node in the set.
isFirstNode :: ClosedTerm (PByteString :--> PSetNode :--> PBool)
isFirstNode = phoistAcyclic $
  plam $ \key node -> P.do
    keys <- pletFields @'["key", "next"] node
    pmatch (keys.next) $ \case
      PKey n ->
        key #== pfromData (pfield @"_0" # n) #&& isNothing # pfromData keys.key
      _ -> pcon PFalse

-- | Checks that a PubkeyHash does belong to the last Node in a set.
isLastNode :: ClosedTerm (PByteString :--> PSetNode :--> PBool)
isLastNode = phoistAcyclic $
  plam $ \key node -> P.do
    keys <- pletFields @'["key", "next"] node
    pmatch (keys.key) $ \case
      PKey (pfield0 -> n) ->
        key #== pfromData n #&& isNothing # pfromData keys.next
      _ -> pcon PFalse

-- | Checks that node key is absent.
isNothing :: Term s (PNodeKey :--> PBool)
isNothing = phoistAcyclic $
  plam $ \md -> pmatch md $ \case
    PKey _ -> pcon PFalse
    PEmpty _ -> pcon PTrue

{- | @
  node `asPredecessorOf` next
  @ makes @node@ to be a predecessor of a node with *key* @next@
  Seen as if the node between them was removed.
  @node.key@ remains the same, @node.next@ changes to @next@.
-}
asPredecessorOf :: ClosedTerm (PAsData PSetNode :--> PByteString :--> PSetNode)
asPredecessorOf = phoistAcyclic $
  plam $ \node next ->
    let nodeKey = pfromData $ pfield @"key" # node
        nextPK = pcon $ PKey $ pdcons @"_0" # pdata next #$ pdnil
     in mkNode # nodeKey # nextPK

{- | @
    key `asSuccessorOf` node
  @ makes @node@ to be a successor of a node with *next* @key@
  Seen as if the node between them was removed.
  @node.next@ remains the same, @node.key@ changes to @key@.
-}
asSuccessorOf :: ClosedTerm (PByteString :--> PAsData PSetNode :--> PSetNode)
asSuccessorOf = phoistAcyclic $
  plam $ \key node ->
    let nodeNext = pfromData $ pfield @"next" # node
        keyPK = pcon $ PKey $ pdcons @"_0" # pdata key #$ pdnil
     in mkNode # keyPK # nodeNext

-- | Extracts the next node key
getNextPK :: ClosedTerm (PAsData PSetNode :--> PMaybe PPubKeyHash)
getNextPK = phoistAcyclic $
  plam $ \node ->
    let nextNodeKey = pfromData $ pfield @"next" # node
     in pmatch nextNodeKey $ \case
          PEmpty _ -> pcon PNothing
          PKey (pfield0 -> n) -> pcon $ PJust $ pcon $ PPubKeyHash $ pfromData n

-- | Extracts the node key
getCurrentPK :: ClosedTerm (PAsData PSetNode :--> PMaybe PPubKeyHash)
getCurrentPK = phoistAcyclic $
  plam $ \node ->
    let nodeKey = pfromData $ pfield @"key" # node
     in pmatch nodeKey $ \case
          PEmpty _ -> pcon PNothing
          PKey (pfield0 -> n) -> pcon $ PJust $ pcon $ PPubKeyHash $ pfromData n

{- | Checks whether @SetNode@ key is less than next node key.
  Any valid sequence of nodes MUST follow this property.
-}
validNode :: ClosedTerm (PAsData PSetNode :--> PBool)
validNode = phoistAcyclic $
  plam $ \node -> P.do
    nodeDatum <- pletFields @'["key", "next"] node
    pmatch (nodeDatum.key) $ \case
      PEmpty _ -> pcon PTrue
      PKey (pfield0 -> key) -> pmatch (nodeDatum.next) $ \case
        PEmpty _ -> pcon PTrue
        PKey (pfield0 -> next) ->
          pfromData key #< pfromData next -- nodes ordered incrementally
