{-# LANGUAGE MonoLocalBinds #-}

module Language.Haskell.Tools.Refactor.Builtin ( builtinRefactorings, builtinQueries ) where

import Language.Haskell.Tools.Refactor (RefactoringChoice, QueryChoice)
import Language.Haskell.Tools.Refactor.Builtin.ExtractBinding (extractBindingRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.FloatOut (floatOutRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.GenerateExports (generateExportsRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.GenerateTypeSignature (generateTypeSignatureRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.InlineBinding (inlineBindingRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions (organizeExtensionsRefactoring, projectOrganizeExtensionsRefactoring, highlightExtensionsQuery)
import Language.Haskell.Tools.Refactor.Builtin.OrganizeImports (organizeImportsRefactoring, projectOrganizeImportsRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.RenameDefinition (renameDefinitionRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.GetMatches (getMatchesQuery)
import Language.Haskell.Tools.Refactor.Builtin.LetRefactoring (changeReturnToLetTypeRefactoring, projectChangePureToLetRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.ShowTree (showAstTree)
import Language.Haskell.Tools.Refactor.Builtin.ChangeMonad (changeMonadRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.WriteBack (writeBackRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.AutoCorrect

builtinRefactorings :: [RefactoringChoice]
builtinRefactorings
  = [ organizeImportsRefactoring
    , projectOrganizeImportsRefactoring
    , inlineBindingRefactoring
    , generateTypeSignatureRefactoring
    , renameDefinitionRefactoring
    , generateExportsRefactoring
    , floatOutRefactoring
    , extractBindingRefactoring
    , organizeExtensionsRefactoring
    , projectOrganizeExtensionsRefactoring
    , autoCorrectRefactoring
    , changeReturnToLetTypeRefactoring
    , projectChangePureToLetRefactoring
    , changeMonadRefactoring
    , showAstTree
    , writeBackRefactoring
    ]

builtinQueries :: [QueryChoice]
builtinQueries = [ getMatchesQuery, highlightExtensionsQuery ]
