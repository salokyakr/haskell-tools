{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- | A module for displaying debug info about the source annotations of the syntax tree in different phases.
module Language.Haskell.Tools.Debug.RangeDebug where

import Control.Reference ()
import GHC.Generics
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.BackendGHC ()
import Language.Haskell.Tools.PrettyPrint.Prepare ()
import Language.Haskell.Tools.Debug.Show ()

import Debug.Trace (trace)

type ShowSrcInfo st = (Show (SpanInfo st), Show (ListInfo st), Show (OptionalInfo st)) 

srcInfoDebug :: TreeDebug e dom st => e dom st -> String
srcInfoDebug = treeDebug' 0

class (ShowSrcInfo st, Domain dom, Show (e dom st))
        => TreeDebug e dom st where
  treeDebug' :: Int -> e dom st -> String
  default treeDebug' :: (GTreeDebug (Rep (e dom st)), Generic (e dom st), Domain dom) => Int -> e dom st -> String
  treeDebug' i = gTreeDebug i . from
  treeTraversal :: e dom st -> e dom st
  -- default treeTraversal :: e dom st -> e dom st 
  -- treeTraversal x = x
  default treeTraversal :: (GTreeTraverse (Rep (e dom st)) e dom st, Generic (e dom st), Domain dom) => e dom st -> e dom st
  treeTraversal = gTreeTraversal . from

class GTreeDebug f where
  gTreeDebug :: Int -> f p -> String
  
class GTreeTraverse f e dom st where
  gTreeTraversal :: f p -> e dom st



instance GTreeDebug V1 where
  gTreeDebug _ = error "GTreeDebug V1"

instance GTreeTraverse V1 e dom st where
  gTreeTraversal _ = error "GTreeDebug V1"



instance GTreeDebug U1 where
  gTreeDebug _ U1 = ""

instance GTreeTraverse U1 e dom st where
  gTreeTraversal U1 = trace (show U1) $ undefined



instance (GTreeDebug f, GTreeDebug g) => GTreeDebug (f :+: g) where
  gTreeDebug i (L1 x) = gTreeDebug i x
  gTreeDebug i (R1 x) = gTreeDebug i x

instance (GTreeTraverse f e dom st, GTreeTraverse g e dom st) => GTreeTraverse (f :+: g) e dom st where
  gTreeTraversal (L1 x) = gTreeTraversal x
  gTreeTraversal (R1 x) = gTreeTraversal x



instance (GTreeDebug f, GTreeDebug g) => GTreeDebug (f :*: g) where
  gTreeDebug i (x :*: y) = gTreeDebug i x ++ gTreeDebug i y

instance (GTreeTraverse f e dom st, GTreeTraverse g e dom st) => GTreeTraverse (f :*: g) e dom st where
  gTreeTraversal (x :*: y) = (gTreeTraversal x)



instance {-# OVERLAPPING #-} (TreeDebug e dom st) => GTreeDebug (K1 i (e dom st)) where
  gTreeDebug i (K1 x) = treeDebug' i x

instance {-# OVERLAPPING #-} (TreeDebug e dom st) => GTreeTraverse (K1 i (e dom st)) e dom st where
  gTreeTraversal (K1 x) = treeTraversal x
  



instance {-# OVERLAPPABLE #-} GTreeDebug (K1 i c) where
  gTreeDebug _ (K1 _) = ""

instance {-# OVERLAPPABLE #-} (Show c) => GTreeTraverse (K1 i c) e dom st where
  gTreeTraversal p@(K1 x) = trace (show (p)) $ undefined


instance GTreeDebug f => GTreeDebug (M1 i t f) where
  gTreeDebug i (M1 x) = gTreeDebug i x

instance GTreeTraverse f e dom st => GTreeTraverse (M1 i t f) e dom st where
  gTreeTraversal (M1 x) = gTreeTraversal x