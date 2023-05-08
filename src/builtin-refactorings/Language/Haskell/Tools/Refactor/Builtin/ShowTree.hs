
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.ShowTree where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Debug.RangeDebug
import Language.Haskell.Tools.Debug.RangeDebugInstances
import Control.Monad.IO.Class

showAstTree :: RefactoringChoice
showAstTree = ModuleRefactoring "showTree" (localRefactoring printAstTree)

printAstTree :: LocalRefactoring
printAstTree moduleAST =
        do
            !_ <- liftIO $ putStrLn "--- Printing Tree ---"
            -- !_ <- liftIO $ putStrLn $ srcInfoDebug moduleAST
            return $ moduleAST