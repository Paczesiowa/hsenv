module Actions ( cabalUpdate
               , installCabalConfig
               , installCabalWrapper
               , installActivateScript
               , copyBaseSystem
               , initGhcDb
               , installGhc
               , createDirStructure
               ) where

import System.Directory (setCurrentDirectory, getCurrentDirectory, createDirectory, removeDirectoryRecursive)
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
import Util.IO (makeExecutable, createTemporaryDirectory)
import Skeletons

-- update cabal package info inside Virtual Haskell Environmentn
cabalUpdate :: MyMonad ()
cabalUpdate = do
  env         <- getVirtualEnvironment
  cabalConfig <- cabalConfigLocation
  info "Updating cabal package database inside Virtual Haskell Environment."
  _ <- indentMessages $ runProcess (Just env) "cabal" ["--config-file=" ++ cabalConfig, "update"] Nothing
  return ()

-- install cabal wrapper (in bin/ directory) inside virtual environment dir structure
installCabalWrapper :: MyMonad ()
installCabalWrapper = do
  cabalConfig      <- cabalConfigLocation
  dirStructure     <- vheDirStructure
  let cabalWrapper = virthualEnvBinDir dirStructure </> "cabal"
  info $ concat [ "Installing cabal wrapper using "
                , cabalConfig
                , " at "
                , cabalWrapper
                ]
  let cabalWrapperContents = substs [("<CABAL_CONFIG>", cabalConfig)] cabalWrapperSkel
  indentMessages $ do
    trace "cabal wrapper contents:"
    indentMessages $ mapM_ trace $ lines cabalWrapperContents
  liftIO $ writeFile cabalWrapper cabalWrapperContents
  liftIO $ makeExecutable cabalWrapper

installActivateScriptSupportFiles :: MyMonad ()
installActivateScriptSupportFiles = do
  debug "installing supporting files"
  dirStructure <- vheDirStructure
  ghc <- asks ghcSource
  indentMessages $ do
    let pathVarPrependixLocation = virthualEnvDir dirStructure </> "path_var_prependix"
        pathVarElems =
            case ghc of
              System    -> [virthualEnvBinDir dirStructure, cabalBinDir dirStructure]
              Tarball _ -> [ virthualEnvBinDir dirStructure
                          , cabalBinDir dirStructure
                          , ghcBinDir dirStructure
                          ]
        pathVarPrependix = intercalate ":" pathVarElems
    debug $ "installing path_var_prependix file to " ++ pathVarPrependixLocation
    indentMessages $ trace $ "path_var_prependix contents: " ++ pathVarPrependix
    liftIO $ writeFile pathVarPrependixLocation pathVarPrependix
    ghcPkgDbPath <- indentMessages ghcPkgDbPathLocation
    let ghcPackagePathVarLocation = virthualEnvDir dirStructure </> "ghc_package_path_var"
        ghcPackagePathVar         = ghcPkgDbPath
    debug $ "installing ghc_package_path_var file to " ++ ghcPackagePathVarLocation
    indentMessages $ trace $ "path_var_prependix contents: " ++ ghcPackagePathVar
    liftIO $ writeFile ghcPackagePathVarLocation ghcPackagePathVar

-- install activate script (in bin/ directory) inside virtual environment dir structure
installActivateScript :: MyMonad ()
installActivateScript = do
  info "Installing activate script"
  virthualEnvName <- asks vheName
  dirStructure    <- vheDirStructure
  ghcPkgDbPath    <- indentMessages ghcPkgDbPathLocation
  let activateScript = virthualEnvBinDir dirStructure </> "activate"
  indentMessages $ debug $ "using location: " ++ activateScript
  let activateScriptContents = substs [ ("<VIRTHUALENV_NAME>", virthualEnvName)
                                      , ("<VIRTHUALENV_DIR>", virthualEnvDir dirStructure)
                                      , ("<VIRTHUALENV>", virthualEnv dirStructure)
                                      , ("<GHC_PACKAGE_PATH>", ghcPkgDbPath)
                                      , ("<VIRTHUALENV_BIN_DIR>", virthualEnvBinDir dirStructure)
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
  cabalConfig     <- cabalConfigLocation
  dirStructure    <- vheDirStructure
  info $ "Installing cabal config at " ++ cabalConfig
  let cabalConfigContents = substs [ ("<GHC_PACKAGE_PATH>", ghcPackagePath dirStructure)
                                   , ("<CABAL_DIR>", cabalDir dirStructure)
                                   ] cabalConfigSkel
  indentMessages $ do
    trace "cabal config contents:"
    indentMessages $ mapM_ trace $ lines cabalConfigContents
  liftIO $ writeFile cabalConfig cabalConfigContents

createDirStructure :: MyMonad ()
createDirStructure = do
  dirStructure <- vheDirStructure
  info "Creating Virtual Haskell directory structure"
  indentMessages $ do
    debug $ "virthualenv directory: " ++ virthualEnvDir dirStructure
    liftIO $ createDirectory $ virthualEnvDir dirStructure
    debug $ "cabal directory: " ++ cabalDir dirStructure
    liftIO $ createDirectory $ cabalDir dirStructure
    debug $ "virthualenv bin directory: " ++ virthualEnvBinDir dirStructure
    liftIO $ createDirectory $ virthualEnvBinDir dirStructure

-- initialize private GHC package database inside virtual environment
initGhcDb :: MyMonad ()
initGhcDb = do
  dirStructure <- vheDirStructure
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
    System              -> indentMessages $ debug "Using system version of GHC - nothing to install."
    Tarball tarballPath -> indentMessages $ installExternalGhc tarballPath

installExternalGhc :: FilePath -> MyMonad ()
installExternalGhc tarballPath = do
  info $ "Installing GHC from " ++ tarballPath
  indentMessages $ do
    dirStructure <- vheDirStructure
    tmpGhcDir <- liftIO $ createTemporaryDirectory (virthualEnv dirStructure) "ghc"
    debug $ "Unpacking GHC tarball to " ++ tmpGhcDir
    _ <- indentMessages $ runProcess Nothing "tar" ["xf", tarballPath, "-C", tmpGhcDir, "--strip-components", "1"] Nothing
    let configureScript = tmpGhcDir </> "configure"
    debug $ "Configuring GHC with prefix " ++ ghcDir dirStructure
    cwd <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory tmpGhcDir
    make <- asks makeCmd
    let configureAndInstall = do
          _ <- indentMessages $ runProcess Nothing configureScript ["--prefix=" ++ ghcDir dirStructure] Nothing
          debug $ "Installing GHC with " ++ make ++ " install"
          _ <- indentMessages $ runProcess Nothing make ["install"] Nothing
          return ()
    configureAndInstall `finally` liftIO (setCurrentDirectory cwd)
    liftIO $ removeDirectoryRecursive tmpGhcDir
    return ()
