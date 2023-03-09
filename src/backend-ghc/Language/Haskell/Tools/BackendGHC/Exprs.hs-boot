{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Tools.BackendGHC.Exprs where

import GHC.Hs.Expr as GHC (HsExpr, HsCmd)
import Language.Haskell.Tools.AST (Ann, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Monad (Trf)
import Language.Haskell.Tools.BackendGHC.Names (TransformName(..))
import SrcLoc as GHC (Located)
import GHC.Hs.Extension (GhcPass)

trfExpr :: (TransformName n r, n ~ GhcPass p)  => Located (HsExpr n) -> Trf (Ann AST.UExpr (Dom r) RangeStage)
trfExpr' :: (TransformName n r, n ~ GhcPass p)  => HsExpr n -> Trf (AST.UExpr (Dom r) RangeStage)
trfCmd' :: (TransformName n r, n ~ GhcPass p)  => HsCmd n -> Trf (AST.UCmd (Dom r) RangeStage)