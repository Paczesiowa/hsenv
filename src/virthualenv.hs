import System.Environment (getProgName, getArgs, getEnvironment)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import System.Process (readProcess, runInteractiveProcess, waitForProcess)
import System.Directory (getCurrentDirectory, createDirectory, removeDirectoryRecursive, setCurrentDirectory)
import System.FilePath ((</>), splitPath)
import Data.List (isPrefixOf)
import Control.Monad
import Distribution.Version
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import qualified Codec.Archive.Tar as Tar
import Codec.Compression.BZip
import qualified Data.ByteString.Lazy as BS

import Skeletons
import Util.IO (getEnvVar, makeExecutable)
import Util.Cabal (parseVersion)
import Util.Template (substs)
import Types
import MyMonad
import Process
import Paths
import PackageManagement

-- check if any virtual env is already active
checkVHE :: IO Bool
checkVHE = do
    virthualEnvVar <- getEnvVar "VIRTHUALENV"
    case virthualEnvVar of
        Nothing   -> return False
        Just path -> do
            virthualEnvName <- getEnvVar "VIRTHUALENV_NAME"
            case virthualEnvName of
                Nothing -> do
                       hPutStrLn stderr $
                           "warning: VIRTHUALENV environment variable is defined"
                        ++ ", but no VIRHTUALENV_NAME environment variable defined."
                       putStrLn $ "There is already active Virtual Haskell Environment (at " ++ path ++ ")."
                Just name -> do
                    putStrLn $ "There is already active " ++ name ++ " Virtual Haskell Environment (at " ++ path ++ ")."
            return True

usage :: IO ()
usage = do
    name <- getProgName
    putStrLn $ "usage: " ++ name ++ " [FLAGS]"
    putStrLn ""
    putStrLn "Flags:"
    putStrLn "-h --help Show this help message"
    putStrLn "--verbose Print some debugging info"
    putStrLn "--name=NAME Use Name for name of Virthual Haskell Environment"
    putStrLn "            (defaults to the name of the current directory)"
    putStrLn "--ghc=X     X can be one of:"
    putStrLn "              system - Virtual Haskell Environment will use"
    putStrLn "                       system copy of ghc (and cabal)"
    putStrLn "                       this is the default behaviour"
    putStrLn "              FILE   - where FILE is a path to a tarball with"
    putStrLn "                       ghc (e.g. ghc-7.0.4-i386-unknown-linux.tar.bz2)"
    putStrLn ""
    putStrLn "Creates Virtual Haskell Environment in the current directory."
    putStrLn "All files will be stored in the .virthualenv/ subdirectory."

parseArgs :: [String] -> IO (Maybe Options)
parseArgs args = do
  let (verbosityFlags, nonVerbosityFlags) = span (`elem` ["--verbose", "--very-verbose"]) args
      verboseness = case verbosityFlags of
                      "--verbose":_      -> Verbose
                      "--very-verbose":_ -> VeryVerbose
                      _                  -> Quiet
      (nameFlags, nonNameFlags) = span ("--name=" `isPrefixOf`) nonVerbosityFlags
  name <- case nameFlags of
           nameFlag:_ -> return $ drop (length "--name=") nameFlag
           [] -> do
             cwd <- liftIO getCurrentDirectory
             let dirs = splitPath cwd
                 name = last dirs
             when (verboseness > Quiet) $ putStrLn $ "Using current directory name as Virtual Haskell Environment name: " ++ name
             return name
  let (ghcSourceFlags, restOfFlags) = span ("--ghc=" `isPrefixOf`) nonNameFlags
      ghc = case ghcSourceFlags of
              []     -> System
              path:_ -> Tarball $ drop (length "--ghc=") path
  case restOfFlags of
    [] -> return $ Just Options { verbosity = verboseness
                               , vheName   = name
                               , ghcSource = ghc
                               }
    _ -> return Nothing

cabalUpdate :: MyMonad ()
cabalUpdate = do
  env <- liftIO getEnvironment
  cabalConfig <- cabalConfigLocation
  dirStructure <- vheDirStructure
  let env' = ("GHC_PACKAGE_PATH", ghcPackagePath dirStructure) : filter (\(k,_) -> k /= "GHC_PACKAGE_PATH") env
  liftIO $ putStrLn "Updating cabal package database inside Virtual Haskell Environment."
  (_, _, _, pid) <-
      liftIO $ runInteractiveProcess "cabal"
                            ["--config-file=" ++ cabalConfig, "update"]
                            Nothing
                            (Just env')
  _ <- liftIO $ waitForProcess pid
  return ()

-- install cabal wrapper (in bin/ directory) inside virtual environment dir structure
installCabalWrapper :: MyMonad ()
installCabalWrapper = do
  cabalConfig      <- cabalConfigLocation
  dirStructure     <- vheDirStructure
  let cabalWrapper = virthualEnvBinDir dirStructure </> "cabal"
  liftIO $ putStrLn $ concat [ "Installing cabal wrapper using "
                             , cabalConfig
                             , " at "
                             , cabalWrapper
                             ]
  let cabalWrapperContents = substs [ ("<CABAL_CONFIG>", cabalConfig)
                                    ] cabalWrapperSkel
  liftIO $ writeFile cabalWrapper cabalWrapperContents
  liftIO $ makeExecutable cabalWrapper

-- install cabal wrapper (in bin/ directory) inside virtual environment dir structure
installActivateScript :: MyMonad ()
installActivateScript = do
  virthualEnvName <- asks vheName
  dirStructure    <- vheDirStructure
  ghc <- asks ghcSource
  ghcPkgPath <-
      case ghc of
        System    -> return $ ghcPackagePath dirStructure
        Tarball _ -> do
          externalGhcPkgDbPath <- externalGhcPkgDb
          return $ ghcPackagePath dirStructure ++ ":" ++ externalGhcPkgDbPath
  let activateScript = virthualEnvBinDir dirStructure </> "activate"
  liftIO $ putStrLn $ "Installing activate script at " ++ activateScript
  let activateScriptContents = substs [ ("<VIRTHUALENV_NAME>", virthualEnvName)
                                      , ("<VIRTHUALENV>", virthualEnv dirStructure)
                                      , ("<GHC_PACKAGE_PATH>", ghcPkgPath)
                                      , ("<VIRTHUALENV_BIN_DIR>", virthualEnvBinDir dirStructure)
                                      , ("<CABAL_BIN_DIR>", cabalBinDir dirStructure)
                                      , ("<GHC_BIN_DIR>", ghcBinDir dirStructure)
                                      ] activateSkel
  liftIO $ writeFile activateScript activateScriptContents

installCabalConfig :: MyMonad ()
installCabalConfig = do
  cabalConfig     <- cabalConfigLocation
  dirStructure    <- vheDirStructure
  liftIO $ putStrLn $ "Installing cabal config at " ++ cabalConfig
  let cabalConfigContents = substs [ ("<GHC_PACKAGE_PATH>", ghcPackagePath dirStructure)
                                   , ("<CABAL_DIR>", cabalDir dirStructure)
                                   ] cabalConfigSkel
  liftIO $ writeFile cabalConfig cabalConfigContents

createDirStructure :: MyMonad ()
createDirStructure = do
  dirStructure <- vheDirStructure
  liftIO $ putStrLn "Creating Virtual Haskell directory structure"
  indentMessages $ do
    debug $ "virthualenv directory: " ++ virthualEnvDir dirStructure
    liftIO $ createDirectory $ virthualEnvDir dirStructure
    debug $ "cabal directory: " ++ cabalDir dirStructure
    liftIO $ createDirectory $ cabalDir dirStructure
    debug $ "virthualenv bin directory: " ++ virthualEnvBinDir dirStructure
    liftIO $ createDirectory $ virthualEnvBinDir dirStructure
    debug $ "tmp directory: " ++ tmpDir dirStructure
    liftIO $ createDirectory $ tmpDir dirStructure

initGhcDb :: MyMonad ()
initGhcDb = do
  dirStructure <- vheDirStructure
  liftIO $ putStrLn $ "Initializing GHC Package database at " ++ ghcPackagePath dirStructure
  (_, out, _) <- outsideGhcPkg ["--version"]
  let versionString      = last $ words out
      Just version       = parseVersion versionString
      ghc_6_12_1_version = Version [6,12,1] []
  if version < ghc_6_12_1_version then do
      indentMessages $ debug "Detected GHC older than 6.12, initializing GHC_PACKAGE_PATH to file with '[]'"
      liftIO $ writeFile (ghcPackagePath dirStructure) "[]"
   else do
      _ <- outsideGhcPkg ["init", ghcPackagePath dirStructure]
      return ()

copyBaseSystem :: MyMonad ()
copyBaseSystem = do
  liftIO $ putStrLn "Copying necessary packages from original GHC package database"
  indentMessages $ do
    ghc <- asks ghcSource
    case ghc of
      System -> do
        transplantPackage "base"
        transplantPackage "Cabal"
        transplantPackage "haskell98"
        transplantPackage "haskell2010"
        transplantPackage "ghc"
        transplantPackage "ghc-binary"
      Tarball _ -> do
        debug "Using external GHC - nothing to copy, Virtual environment will reuse GHC package database"

main :: IO ()
main = do
    envActive <- checkVHE
    when envActive exitFailure

    args <- getArgs
    case args of
      ["--help"] -> usage
      ["-h"]     -> usage
      _          -> do
                opts <- parseArgs args
                case opts of
                  Nothing      -> usage >> exitFailure
                  Just options -> do
                    result <- runMyMonad realMain options
                    case result of
                      Left err -> hPutStrLn stderr $ getExceptionMessage err
                      Right ()  -> return ()

installGhc :: MyMonad ()
installGhc = do
  debug "Installing GHC"
  ghc          <- asks ghcSource
  case ghc of
    System              -> indentMessages $ debug "Using system version of GHC - nothing to install."
    Tarball tarballPath -> indentMessages $ installExternalGhc tarballPath

installExternalGhc :: FilePath -> MyMonad ()
installExternalGhc tarballPath = do
  liftIO $ putStrLn $ "Installing GHC from " ++ tarballPath
  dirStructure <- vheDirStructure
  let tmpGhcDir = tmpDir dirStructure </> "ghc"
  liftIO $ createDirectory tmpGhcDir
  debug $ "Unpacking GHC tarball to " ++ tmpGhcDir
  _ <- liftIO $ readProcess  "tar" ["xf", tarballPath, "-C", tmpGhcDir, "--strip-components", "1"] ""
  let configureScript = tmpGhcDir </> "configure"
  debug $ "Configuring GHC with prefix " ++ ghcDir dirStructure
  cwd <- liftIO getCurrentDirectory
  liftIO $ setCurrentDirectory tmpGhcDir
  _ <- liftIO $ readProcess configureScript ["--prefix=" ++ ghcDir dirStructure] ""
  debug "Installing GHC"
  _ <- liftIO $ readProcess "make" ["install"] ""
  liftIO $ setCurrentDirectory cwd
  return ()

realMain :: MyMonad ()
realMain = do
  createDirStructure
  installGhc
  initGhcDb
  copyBaseSystem
  installCabalConfig
  installActivateScript
  installCabalWrapper
  cabalUpdate
  dirStructure <- vheDirStructure
  liftIO $ removeDirectoryRecursive $ tmpDir dirStructure
