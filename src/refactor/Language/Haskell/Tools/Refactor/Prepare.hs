{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines utility methods that prepare Haskell modules for refactoring
module Language.Haskell.Tools.Refactor.Prepare where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.List ((\\), isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Language.Haskell.TH.LanguageExtensions (Extension(..))
import System.Directory (canonicalizePath)
import System.FilePath
import Outputable
import DynamicLoading (initializePlugins)

import CmdLineParser (CmdLineP(..), processArgs, Warn(..), Err(..))
import DynFlags
import FastString (mkFastString)
import GHC hiding (loadModule, ModuleName)
import Module (moduleNameFS, moduleNameString)
import qualified GHC (loadModule)
import GHC.Paths ( libdir )
import GhcMonad
import HscTypes
import Outputable (Outputable(..), showSDocUnsafe, cat, (<>))
import Packages (initPackages)
import SrcLoc
import HscMain
import StringBuffer (hGetStringBuffer)

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Refactor.Monad (Refactoring(..))
import Language.Haskell.Tools.Refactor.Representation
import Language.Haskell.Tools.Refactor.Utils.Monadic (runRefactor)

-- | Type synonym for module names.
type ModuleName = String


-- | A quick function to try the refactorings
tryRefactor :: (RealSrcSpan -> Refactoring) -> String -> ModuleName -> IO ()
tryRefactor refact moduleName span
  = runGhc (Just libdir) $ do
      initGhcFlags
      useDirs ["."]
      liftIO $ print "before refactor"
      mod <- loadModule "." moduleName >>= parseTyped
      liftIO $ print "after refactor"
      res <- runRefactor (SourceFileKey (moduleSourceFile moduleName) moduleName, mod) []
               $ refact $ correctRefactorSpan mod $ readSrcSpan span
      case res of Right r -> liftIO $ mapM_ (putStrLn . prettyPrint . snd . fromContentChanged) r
                  Left err -> liftIO $ putStrLn err

-- | Adjust the source range to be applied to the refactored module
correctRefactorSpan :: UnnamedModule -> RealSrcSpan -> RealSrcSpan
correctRefactorSpan mod sp = mkRealSrcSpan (updateSrcFile fileName $ realSrcSpanStart sp)
                                           (updateSrcFile fileName $ realSrcSpanEnd sp)
  where fileName = case srcSpanStart $ getRange mod of RealSrcLoc loc -> srcLocFile loc
                                                       _ -> error "correctRefactorSpan: no real span"
        updateSrcFile fn loc = mkRealSrcLoc fn (srcLocLine loc) (srcLocCol loc)

-- | Set the given flags for the GHC session.
-- Also gives back a change function that you can use to apply the settings to any flags.
-- Prints out errors and warnings
useFlags :: [String] -> Ghc ([String], DynFlags -> DynFlags)
useFlags args = do
  let lArgs = map (L noSrcSpan) args
  dynflags <- getSessionDynFlags
  let change = runCmdLine $ processArgs flagsAll lArgs
  let ((leftovers, errs, warnings), newDynFlags) = change dynflags
  unless (null warnings)
    $ liftIO $ putStrLn $ showSDocUnsafe $ cat $ map pprWarning warnings
  unless (null errs)
    $ liftIO $ putStrLn $ showSDocUnsafe $ cat $ map pprErr errs
  void $ setSessionDynFlags newDynFlags --  { pluginModNames = pluginModNames newDynFlags ++ [mkModuleName "Data.Record.Anon.Plugin", mkModuleName "RecordDotPreprocessor"]})
  when (any ("-package-db" `isSuffixOf`) args) reloadPkgDb
  return (map unLoc leftovers, snd . change)

pprWarning (Warn reason msg) = ppr reason Outputable.<> ppr msg
pprErr (Err msg) = ppr msg

-- | Reloads the package database based on the session flags
reloadPkgDb :: Ghc ()
reloadPkgDb = void $ setSessionDynFlags . fst =<< liftIO . initPackages . (\df -> df { pkgDatabase = Nothing })
                                              =<< getSessionDynFlags

-- | Initialize GHC flags to default values that support refactoring
initGhcFlags :: Ghc ()
initGhcFlags = initGhcFlags' False True

initGhcFlagsForTest :: Ghc ()
initGhcFlagsForTest = do initGhcFlags' True False
                         dfs <- getSessionDynFlags
                         void $ setSessionDynFlags $ dfs { hscTarget = HscAsm
                                                        --  , pluginModNames = pluginModNames dfs ++ [mkModuleName "Data.Record.Anon.Plugin", mkModuleName "RecordDotPreprocessor"]
                                                          }

-- | Sets up basic flags and settings for GHC
initGhcFlags' :: Bool -> Bool -> Ghc ()
initGhcFlags' needsCodeGen errorsSuppressed = do
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ (if errorsSuppressed then flip gopt_set Opt_DeferTypeErrors
                                  . flip gopt_set Opt_DeferTypedHoles
                                  . flip gopt_set Opt_DeferOutOfScopeVariables
                           else id)
    $ dflags { importPaths = []
             , hscTarget = if needsCodeGen then HscInterpreted else HscNothing
             , ghcLink = if needsCodeGen then LinkInMemory else NoLink
             , ghcMode = CompManager
             , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
            --  , pluginModNames = pluginModNames dflags ++ [mkModuleName "Data.Record.Anon.Plugin", mkModuleName "RecordDotPreprocessor"]
             }

-- | Use the given source directories when searching for imported modules
useDirs :: [FilePath] -> Ghc ()
useDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = importPaths dynflags ++ workingDirs }

-- | Don't use the given source directories when searching for imported modules
deregisterDirs :: [FilePath] -> Ghc ()
deregisterDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = importPaths dynflags \\ workingDirs }

-- | Translates module name and working directory into the name of the file where the given module should be defined
toFileName :: FilePath -> ModuleName -> FilePath
toFileName workingDir mod = normalise $ workingDir </> map (\case '.' -> pathSeparator; c -> c) mod ++ ".hs"

-- | Translates module name and working directory into the name of the file where the boot module should be defined
toBootFileName :: FilePath -> ModuleName -> FilePath
toBootFileName workingDir mod = normalise $ workingDir </> map (\case '.' -> pathSeparator; c -> c) mod ++ ".hs-boot"

-- | Get the source directory where the module is located.
getSourceDir :: ModSummary -> IO FilePath
getSourceDir ms
  = do filePath <- canonicalizePath $ getModSumOrig ms
       let modNameParts = splitOn "." $ GHC.moduleNameString (moduleName (ms_mod ms))
           filePathParts = splitPath filePath
       let srcDirParts = reverse $ drop (length modNameParts) $ reverse filePathParts
       return $ joinPath srcDirParts

-- | Gets the path to the source file of the module.
getModSumOrig :: ModSummary -> FilePath
getModSumOrig = normalise . fromMaybe (error "getModSumOrig: The given module doesn't have haskell source file.") . ml_hs_file . ms_location

keyFromMS :: ModSummary -> SourceFileKey
keyFromMS ms = SourceFileKey (normalise $ getModSumOrig ms) (getModSumName ms)

-- | Gets the module name
getModSumName :: ModSummary -> String
getModSumName = GHC.moduleNameString . moduleName . ms_mod

-- | Load the AST of a module given by the working directory and module name.
loadModuleAST :: FilePath -> ModuleName -> Ghc TypedModule
loadModuleAST workingDir moduleName = do
  useFlags ["-w"]
  modSummary <- loadModule workingDir moduleName
  parseTyped modSummary

-- | Load the summary of a module given by the working directory and module name.
loadModule :: FilePath -> ModuleName -> Ghc ModSummary
loadModule workingDir moduleName
  = do initGhcFlagsForTest
       useDirs [workingDir]
       target <- guessTarget moduleName Nothing
       setTargets [target]
       void $ load (LoadUpTo $ mkModuleName moduleName)
       getModSummary $ mkModuleName moduleName

-- | The final version of our AST, with type infromation added
type TypedModule = Ann AST.UModule IdDom SrcTemplateStage

-- | Get the typed representation of a Haskell module.
parseTyped :: ModSummary -> Ghc TypedModule
parseTyped ms'' = do
  hsc_env' <- getSession
  dynflags' <- liftIO (initializePlugins hsc_env' (GHC.ms_hspp_opts ms''))
  let modSum = ms'' { ms_hspp_opts = dynflags' }
  let hasCppExtension = Cpp `xopt` ms_hspp_opts modSum
      ms = modSumNormalizeFlags modSum
  when (ApplicativeDo `xopt` ms_hspp_opts modSum) $ liftIO $ throwIO $ UnsupportedExtension "ApplicativeDo"
  -- when (OverloadedLabels `xopt` ms_hspp_opts modSum) $ liftIO $ throwIO $ UnsupportedExtension "OverloadedLabels"
  when (ImplicitParams `xopt` ms_hspp_opts modSum) $ liftIO $ throwIO $ UnsupportedExtension "ImplicitParams"
  dyn <- getSessionDynFlags
  liftIO $ print $ "before parse: " ++ show (moduleNameFS <$> pluginModNames dyn) ++ " moduleName: " ++ (Module.moduleNameString $ moduleName $ ms_mod ms)
  --hs_env <- getSession
  --v <- liftIO $ runHsc hs_env $ hscFileFrontEnd ms
  --liftIO $ print "after hscFileFrontEnd"
  p <- parseModule ms
  p' <- parseModule ms''
  liftIO $ print $ "after parse: " ++ (Module.moduleNameString $ moduleName $ ms_mod ms) ++ " dynflags: " ++ show (moduleNameFS <$> pluginModNames (ms_hspp_opts $ pm_mod_summary  p))
  liftIO $ print $ "ast parse: " ++ (showSDocUnsafe $ ppr $ pm_parsed_source p)
  tc <- typecheckModule p
  liftIO $ print $ "ast parse: " ++ (showSDocUnsafe $ ppr $ pm_parsed_source p)
  -- liftIO $ print $ "ast parse: " ++ (showSDocUnsafe $ ppr $ pm_mod_summary tc)
  -- liftIO $ print $ "ast parse: " ++ show tc
  void $ GHC.loadModule tc -- when used with loadModule, the module will be loaded twice
  liftIO $ print $ "ast parse: " ++ (showSDocUnsafe $ ppr $ pm_parsed_source p)
  let annots = pm_annotations p'
  liftIO $ print $ "cpp parse: " ++ show hasCppExtension
  srcBuffer <- if hasCppExtension
                    then liftIO $ hGetStringBuffer (getModSumOrig ms'')
                    else return (fromJust $ ms_hspp_buf $ pm_mod_summary p')
  liftIO $ print "after srcBuffer"
  x <- withTempSession (\e -> e { hsc_dflags = ms_hspp_opts ms })
    $ (if hasCppExtension then prepareASTCpp else prepareAST) srcBuffer . placeComments (fst annots) (getNormalComments $ snd annots)
        <$> (addTypeInfos (typecheckedSource tc)
               =<< (do parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule ms (pm_parsed_source p')
                       x <- runTrf (fst annots) (getPragmaComments $ snd annots)
                              $ trfModuleRename ms parseTrf
                                    (fromJust $ tm_renamed_source tc)
                                    (pm_parsed_source p')
                       return x))
  liftIO $ print $ "after srcBuffer" ++ (prettyPrint x)
  return x

data UnsupportedExtension = UnsupportedExtension String
  deriving Show

instance Exception UnsupportedExtension

trfProblem :: String -> a
trfProblem = throw . UnsupportedExtension

-- | Modifies the dynamic flags for performing a ghc task
withAlteredDynFlags :: GhcMonad m => (DynFlags -> m DynFlags) -> m a -> m a
withAlteredDynFlags modDFs action = do
  dfs <- getSessionDynFlags
  newFlags <- modDFs dfs
  void $ modifySession $ \s -> s { hsc_dflags = newFlags }
  res <- action
  void $ modifySession $ \s -> s { hsc_dflags = dfs }
  return res

-- | Forces the code generation for a given module
forceCodeGen :: ModSummary -> ModSummary
forceCodeGen ms = ms { ms_hspp_opts = codeGenDfs (ms_hspp_opts ms) }

codeGenDfs :: DynFlags -> DynFlags
codeGenDfs dfs = dfs { hscTarget = HscInterpreted, ghcLink = LinkInMemory }

-- | Forces ASM code generation for a given module
forceAsmGen :: ModSummary -> ModSummary
forceAsmGen ms = ms { ms_hspp_opts = modOpts' }
  where modOpts = (ms_hspp_opts ms) { hscTarget = defaultObjectTarget (targetPlatform (ms_hspp_opts ms)) }
        modOpts' = modOpts { ghcLink = LinkInMemory }

-- | Normalizes the flags for a module summary
modSumNormalizeFlags :: ModSummary -> ModSummary
modSumNormalizeFlags ms = ms { ms_hspp_opts = normalizeFlags (ms_hspp_opts ms) }

-- | Removes all flags that are unintelligable for refactoring
normalizeFlags :: DynFlags -> DynFlags
normalizeFlags = updOptLevel 0

-- | Read a source range from our textual format: @line:col-line:col@ or @line:col@
readSrcSpan :: String -> RealSrcSpan
readSrcSpan s = case splitOn "-" s of
  [one] -> mkRealSrcSpan (readSrcLoc one) (readSrcLoc one)
  [from,to] -> mkRealSrcSpan (readSrcLoc from) (readSrcLoc to)

-- | Read a source location from our format: @line:col@
readSrcLoc :: String -> RealSrcLoc
readSrcLoc s = case splitOn ":" s of
  [line,col] -> mkRealSrcLoc (mkFastString "file-name-should-be-fixed") (read line) (read col)
  _ -> error "readSrcLoc: panic: splitOn gives empty list"
