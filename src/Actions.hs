module Actions ( cabalUpdate
               , installCabalConfig
               , installCabalWrapper
	       , installGhcPkgWrapper
	       , installGhciWrapper
	       , installGhcWrapper
               , installActivateScript
               , copyBaseSystem
               , initGhcDb
               , installGhc
               , createDirStructure
               ) where

import System.Directory (setCurrentDirectory, getCurrentDirectory, createDirectory, removeDirectoryRecursive, getAppUserDataDirectory, doesFileExist)
import System.FilePath ((</>))
import Distribution.Version (Version (..))
import Distribution.Package (PackageName(..))
import Safe (lastMay)
import Data.List (intercalate)

import MyMonad
import Types
import Paths
import PackageManagement
import Process
import Util.Template (substs)
import Util.IO (makeExecutable, createTemporaryDirectory, which)
import Skeletons

-- update cabal package info inside Virtual Haskell Environment
cabalUpdate :: MyMonad ()
cabalUpdate = do
  noSharingFlag <- asks noSharing
  if noSharingFlag then do
    debug "Sharing user-wide ~/.cabal/packages disabled"
    cabalUpdate'
   else do
    debug "Sharing user-wide ~/.cabal/packages enabled, checking if data is already downloaded"
    cabalInstallDir <- liftIO $ getAppUserDataDirectory "cabal"
    let hackageData = foldl (</>) cabalInstallDir [ "packages"
                                                  , "hackage.haskell.org"
                                                  , "00-index.tar"
                                                  ]
    dataExists <- liftIO $ doesFileExist hackageData
    if dataExists then do
      info "Skipping 'cabal update' step, Hackage download cache already downloaded"
      info "  to ~/.cabal/packages/. You can update it manually with 'cabal update'"
      info "  (from inside or outside the virtual environment)."
     else do
      debug "No user-wide Hackage cache data downloaded"
      cabalUpdate'
      where cabalUpdate' = do
              cabalConfig <- cabalConfigLocation
              info "Updating cabal package database inside Virtual Haskell Environment."
              _ <- indentMessages $ insideProcess "cabal" ["--config-file=" ++ cabalConfig, "update"] Nothing
              return ()


-- install cabal wrapper (in bin/ directory) inside virtual environment dir structure
installCabalWrapper :: MyMonad ()
installCabalWrapper = do
  cabalConfig  <- cabalConfigLocation
  dirStructure <- hseDirStructure
  hsEnvName'   <- asks hsEnvName
  let cabalWrapper = hsEnvBinDir dirStructure </> "cabal"
  info $ concat [ "Installing cabal wrapper using "
                , cabalConfig
                , " at "
                , cabalWrapper
                ]
  let cabalWrapperContents = substs [ ("<CABAL_CONFIG>", cabalConfig)
                                    , ("<HSENV_NAME>", hsEnvName')] cabalWrapperSkel
  indentMessages $ do
    trace "cabal wrapper contents:"
    indentMessages $ mapM_ trace $ lines cabalWrapperContents
  liftIO $ writeFile cabalWrapper cabalWrapperContents
  liftIO $ makeExecutable cabalWrapper

-- install ghc-pkg wrapper (in bin/ directory) inside virtual environment dir structure
installGhcPkgWrapper :: MyMonad ()
installGhcPkgWrapper = do
  dirStructure <- hseDirStructure
  let ghcpkgWrapper = hsEnvBinDir dirStructure </> "ghc-pkg"
  info $ concat [ "Installing ghc-pkg wrapper at"
		,ghcpkgWrapper
		]
  let ghcpkgWrapperContents = substs [("<HSENV_DIR>", hsEnvDir dirStructure)] ghcpkgWrapperSkel
  indentMessages $ do
    trace "ghc-pkg wrapper contents:"
    indentMessages $ mapM_ trace $ lines ghcpkgWrapperContents
  liftIO $ writeFile ghcpkgWrapper ghcpkgWrapperContents
  liftIO $ makeExecutable ghcpkgWrapper

-- install ghci wrapper (in bin/ directory) inside virtual environment dir structure
installGhciWrapper :: MyMonad ()
installGhciWrapper = do
  dirStructure <- hseDirStructure
  let ghciWrapper = hsEnvBinDir dirStructure </> "ghci"
  info $ concat [ "Installing ghci wrapper at"
		,ghciWrapper
		]
  let ghciWrapperContents = substs [("<HSENV_DIR>", hsEnvDir dirStructure)] ghciWrapperSkel
  indentMessages $ do
    trace "ghc-pkg wrapper contents:"
    indentMessages $ mapM_ trace $ lines ghciWrapperContents
  liftIO $ writeFile ghciWrapper ghciWrapperContents
  liftIO $ makeExecutable ghciWrapper

-- install ghc wrapper (in bin/ directory) inside virtual environment dir structure
installGhcWrapper :: MyMonad ()
installGhcWrapper = do
  dirStructure <- hseDirStructure
  let ghcWrapper = hsEnvBinDir dirStructure </> "ghc"
  info $ concat [ "Installing ghc wrapper at"
		,ghcWrapper
		]
  let ghcWrapperContents = substs [("<HSENV_DIR>", hsEnvDir dirStructure)] ghcWrapperSkel
  indentMessages $ do
    trace "ghc-pkg wrapper contents:"
    indentMessages $ mapM_ trace $ lines ghcWrapperContents
  liftIO $ writeFile ghcWrapper ghcWrapperContents
  liftIO $ makeExecutable ghcWrapper

installActivateScriptSupportFiles :: MyMonad ()
installActivateScriptSupportFiles = do
  debug "installing supporting files"
  dirStructure <- hseDirStructure
  ghc          <- asks ghcSource
  indentMessages $ do
    let pathVarPrependixLocation = hsEnvDir dirStructure </> "path_var_prependix"
        pathVarElems =
            case ghc of
              System    -> [hsEnvBinDir dirStructure, cabalBinDir dirStructure]
              Tarball _ -> [ hsEnvBinDir dirStructure
                          , cabalBinDir dirStructure
                          , ghcBinDir dirStructure
                          ]
        pathVarPrependix = intercalate ":" pathVarElems
    debug $ "installing path_var_prependix file to " ++ pathVarPrependixLocation
    indentMessages $ trace $ "path_var_prependix contents: " ++ pathVarPrependix
    liftIO $ writeFile pathVarPrependixLocation pathVarPrependix
    ghcPkgDbPath <- indentMessages ghcPkgDbPathLocation
    let ghcPackagePathVarLocation = hsEnvDir dirStructure </> "ghc_package_path_var"
        ghcPackagePathVar         = ghcPkgDbPath
    debug $ "installing ghc_package_path_var file to " ++ ghcPackagePathVarLocation
    indentMessages $ trace $ "path_var_prependix contents: " ++ ghcPackagePathVar
    liftIO $ writeFile ghcPackagePathVarLocation ghcPackagePathVar

-- install activate script (in bin/ directory) inside virtual environment dir structure
installActivateScript :: MyMonad ()
installActivateScript = do
  info "Installing activate script"
  hsEnvName'   <- asks hsEnvName
  dirStructure <- hseDirStructure
  ghcPkgDbPath <- indentMessages ghcPkgDbPathLocation
  let activateScript = hsEnvBinDir dirStructure </> "activate"
  indentMessages $ debug $ "using location: " ++ activateScript
  let activateScriptContents = substs [ ("<HSENV_NAME>", hsEnvName')
                                      , ("<HSENV_DIR>", hsEnvDir dirStructure)
                                      , ("<HSENV>", hsEnv dirStructure)
                                      , ("<GHC_PACKAGE_PATH>", ghcPkgDbPath)
                                      , ("<HSENV_BIN_DIR>", hsEnvBinDir dirStructure)
                                      , ("<CABAL_BIN_DIR>", cabalBinDir dirStructure)
                                      , ("<GHC_BIN_DIR>", ghcBinDir dirStructure)
                                      ] activateSkel
  indentMessages $ do
    trace "activate script contents:"
    indentMessages $ mapM_ trace $ lines activateScriptContents
  liftIO $ writeFile activateScript activateScriptContents
  indentMessages installActivateScriptSupportFiles

-- install cabal's config file (in cabal/ directory) inside virtual environment dir structure
installCabalConfig :: MyMonad ()
installCabalConfig = do
  cabalConfig  <- cabalConfigLocation
  dirStructure <- hseDirStructure
  noSharingFlag  <- asks noSharing
  hackageCache <- indentMessages $
      if noSharingFlag then do
          info "Using private Hackage download cache directory"
          return $ cabalDir dirStructure </> "packages"
      else do
          info "Using user-wide (~/.cabal/packages) Hackage download cache directory"
          cabalInstallDir <- liftIO $ getAppUserDataDirectory "cabal"
          return $ cabalInstallDir </> "packages"
  info $ "Installing cabal config at " ++ cabalConfig
-- always use a specific compiler to avoid cabal using ghc wrapper
  program <- ghcbinPath
  compilerdir <- case program of
	Nothing -> do
		_ <- throwError $ MyException $ unwords ["No", "ghc", "in $PATH"]
		return ""
	Just pth -> do
		return pth
  let cabalConfigContents = substs [ ("<GHC_PACKAGE_PATH>", ghcPackagePath dirStructure)
                                   , ("<CABAL_DIR>", cabalDir dirStructure)
                                   , ("<HACKAGE_CACHE>", hackageCache)
                                   , ("<COMPILER_DIR>", compilerdir)
                                   ] cabalConfigSkel
  indentMessages $ do
    trace "cabal config contents:"
    indentMessages $ mapM_ trace $ lines cabalConfigContents
  liftIO $ writeFile cabalConfig cabalConfigContents

ghcbinPath::MyMonad (Maybe FilePath)
ghcbinPath= do
	dirStructure <- hseDirStructure
	ghc <- asks ghcSource
	case ghc of
		System -> do
			ghcbinpath <- liftIO $ which Nothing "ghc"
			return ghcbinpath
		Tarball _ -> do
			return $ Just $ ghcDir dirStructure </> "bin" </> "ghc"


createDirStructure :: MyMonad ()
createDirStructure = do
  dirStructure <- hseDirStructure
  info "Creating Virtual Haskell directory structure"
  indentMessages $ do
    debug $ "hsenv directory: " ++ hsEnvDir dirStructure
    liftIO $ createDirectory $ hsEnvDir dirStructure
    debug $ "cabal directory: " ++ cabalDir dirStructure
    liftIO $ createDirectory $ cabalDir dirStructure
    debug $ "hsenv bin directory: " ++ hsEnvBinDir dirStructure
    liftIO $ createDirectory $ hsEnvBinDir dirStructure

-- initialize private GHC package database inside virtual environment
initGhcDb :: MyMonad ()
initGhcDb = do
  dirStructure <- hseDirStructure
  info $ "Initializing GHC Package database at " ++ ghcPackagePath dirStructure
  out <- indentMessages $ outsideGhcPkg ["--version"]
  case lastMay $ words out of
    Nothing            -> throwError $ MyException $ "Couldn't extract ghc-pkg version number from: " ++ out
    Just versionString -> do
      indentMessages $ trace $ "Found version string: " ++ versionString
      version <- parseVersion versionString
      let ghc_6_12_1_version = Version [6,12,1] []
      if version < ghc_6_12_1_version then do
        indentMessages $ debug "Detected GHC older than 6.12, initializing GHC_PACKAGE_PATH to file with '[]'"
        liftIO $ writeFile (ghcPackagePath dirStructure) "[]"
       else do
        _ <- indentMessages $ outsideGhcPkg ["init", ghcPackagePath dirStructure]
        return ()

-- copy optional packages and don't fail completely if this copying fails
-- some packages mail fail to copy and it's not fatal (e.g. older GHCs don't have haskell2010)
transplantOptionalPackage :: String -> MyMonad ()
transplantOptionalPackage name = transplantPackage (PackageName name) `catchError` handler
  where handler e = do
          warning $ "Failed to copy optional package " ++ name ++ " from system's GHC: "
          indentMessages $ warning $ getExceptionMessage e

-- copy base system
-- base - needed for ghci and everything else
-- Cabal - needed to install non-trivial cabal packages with cabal-install
-- haskell98 - some packages need it but they don't specify it (seems it's an implicit dependancy)
-- haskell2010 - maybe it's similar to haskell98?
-- ghc and ghc-binary - two packages that are provided with GHC and cannot be installed any other way
-- also include dependant packages of all the above
-- when using GHC from tarball, just reuse its package database
-- cannot do the same when using system's GHC, because there might be additional packages installed
-- then it wouldn't be possible to work on them insie virtual environment
copyBaseSystem :: MyMonad ()
copyBaseSystem = do
  info "Copying necessary packages from original GHC package database"
  indentMessages $ do
    ghc <- asks ghcSource
    case ghc of
      System -> do
        transplantPackage $ PackageName "base"
        transplantPackage $ PackageName "Cabal"
        mapM_ transplantOptionalPackage ["haskell98", "haskell2010", "ghc", "ghc-binary"]
      Tarball _ ->
        debug "Using external GHC - nothing to copy, Virtual environment will reuse GHC package database"

installGhc :: MyMonad ()
installGhc = do
  info "Installing GHC"
  ghc <- asks ghcSource
  case ghc of
    System              -> do
	installGhcWrapper
	indentMessages $ debug "Using system version of GHC - nothing to install."
    Tarball tarballPath -> indentMessages $ installExternalGhc tarballPath

installExternalGhc :: FilePath -> MyMonad ()
installExternalGhc tarballPath = do
  info $ "Installing GHC from " ++ tarballPath
  indentMessages $ do
    dirStructure <- hseDirStructure
    tmpGhcDir <- liftIO $ createTemporaryDirectory (hsEnv dirStructure) "ghc"
    debug $ "Unpacking GHC tarball to " ++ tmpGhcDir
    _ <- indentMessages $ outsideProcess' "tar" ["xf", tarballPath, "-C", tmpGhcDir, "--strip-components", "1"]
    let configureScript = tmpGhcDir </> "configure"
    debug $ "Configuring GHC with prefix " ++ ghcDir dirStructure
    cwd <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory tmpGhcDir
    make <- asks makeCmd
    let configureAndInstall = do
          _ <- indentMessages $ outsideProcess' configureScript ["--prefix=" ++ ghcDir dirStructure]
          debug $ "Installing GHC with " ++ make ++ " install"
          _ <- indentMessages $ outsideProcess' make ["install"]
          return ()
    configureAndInstall `finally` liftIO (setCurrentDirectory cwd)
    liftIO $ removeDirectoryRecursive tmpGhcDir
    return ()
