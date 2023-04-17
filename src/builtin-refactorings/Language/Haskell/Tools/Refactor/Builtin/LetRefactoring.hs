
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.LetRefactoring where

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

changeReturnToLetTypeRefactoring :: RefactoringChoice
changeReturnToLetTypeRefactoring = ModuleRefactoring "changeReturnPureToLet" (localRefactoring changeReturnToLetType)

projectChangePureToLetRefactoring :: RefactoringChoice
projectChangePureToLetRefactoring = ProjectRefactoring "projectChangePureToLetRefactoring" projectPureToLetRefactoring

projectPureToLetRefactoring :: ProjectRefactoring
projectPureToLetRefactoring mods
  = mapM (\(k, m) -> ContentChanged . (k,) <$> localRefactoringRes id m (changeReturnToLetType m)) mods


changeReturnToLetType :: LocalRefactoring
changeReturnToLetType moduleAST =
        do
            newAST <- liftGhc $ (!~) (modDecl & annList & declValBind & (valBindRhs &+& valBindLocals & annJust & localBinds & annList & localVal & valBindRhs) & rhsExpr & exprStmts & annList) (bindStmtToLetStmt) (moduleAST)
            return $ newAST

bindStmtToLetStmt :: Stmt -> Ghc Stmt 
bindStmtToLetStmt stmt@(BindStmt pat expr) =
    do 
        (shouldUpdate, updatedRhsExpr) <- checkAndRemoveIfDefFun expr 
        if shouldUpdate
            then 
                let updatedRhs = mkUnguardedRhs $ updatedRhsExpr
                    lclbnd = mkLocalValBind $ mkSimpleBind pat updatedRhs Nothing 
                in return $ mkLetStmt [lclbnd]
        else return $ stmt
bindStmtToLetStmt stmt = return $ stmt

checkAndRemoveIfDefFun :: Expr -> Ghc (Bool, Expr)
checkAndRemoveIfDefFun expr@(App fun args) =
    do 
        let arr = (fun ^? (exprName & simpleName & unqualifiedName & simpleNameStr)) 
            y = (head arr)
            defFun = ["return", "pure"]
        if y `elem` defFun
            then return $ (True, args)
        else return $ (False, expr)
checkAndRemoveIfDefFun expr                = return $ (False, expr)   
