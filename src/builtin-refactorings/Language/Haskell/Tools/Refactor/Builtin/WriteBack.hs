
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Tools.Refactor.Builtin.WriteBack where

import Language.Haskell.Tools.Refactor as Refactor hiding (LambdaCase)
import Language.Haskell.Tools.Refactor.Utils.Extensions
import Language.Haskell.Tools.Rewrite.Match.Binds

import GHC (Ghc(..))
import Module as GHC
import InstEnv as GHC
import Unify as GHC
import Type as GHC
import Name as GHC
import Var as GHC
import UniqSupply as GHC
import Unique as GHC
import TysWiredIn as GHC
import TysPrim as GHC
import PrelNames as GHC
import ConLike as GHC
import PatSyn as GHC
import BasicTypes as GHC

import Control.Reference
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.List
import qualified Data.Map.Strict as SMap (empty, toList)

import Debug.Trace (trace, traceShowId)

import Outputable
import Language.Haskell.Tools.PrettyPrint

import Language.Haskell.Tools.BackendGHC.Exprs (trfExpr)
import Language.Haskell.Tools.BackendGHC.GHCUtils (occName, fromSrcText)
import Language.Haskell.Tools.BackendGHC.Names
import Language.Haskell.Tools.BackendGHC.Patterns (trfPattern)
import Language.Haskell.Tools.BackendGHC.Types (trfType)

import Language.Haskell.Tools.AST (Ann, AnnMaybeG, AnnListG, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

import Control.Monad.IO.Class


-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again

--       Pretty easy now. Chcek wheter it is already in the ExtMap.

writeBackRefactoring :: RefactoringChoice
writeBackRefactoring = ModuleRefactoring "writeBack" (localRefactoring writeBack)


writeBack :: LocalRefactoring
writeBack moduleAST =
        do
            !newAST <- liftGhc $ (!~) (biplateRef) (logicFn) (moduleAST)
            (AST.AnnListG annot currentDecl) <- newAST ^? (modDecl) 
            let newModDecl = filter check currentDecl 
            return $ (.=) modDecl (AST.AnnListG annot newModDecl) newAST

check :: Decl -> Bool 
check (InstanceDecl (InstanceRule _ _ (AppInstanceHead fn _)) _) =
                                                let (x :: [String]) = fn ^? biplateRef
                                                in not $ any (\x -> x == "HasField") x    
check _ = True 

logicFn :: Expr -> Ghc Expr 
logicFn expr@(App (ExplicitTypeApp (Var e1) (PromotedStringType ty)) (Var arg)) = do 
                            let fnName  = showName e1  
                                objName = showName arg
                            if fnName == "getField"
                                then return $ mkVar $ mkName $ (objName ++ "." ++ ty)
                            else return expr
logicFn x = return x  