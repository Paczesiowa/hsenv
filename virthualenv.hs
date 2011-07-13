import System.Environment (getEnv, getProgName, getArgs, getEnvironment)
import System.IO.Error (isDoesNotExistError)
import System.Exit (exitFailure)
import System.Process (readProcess, runInteractiveProcess, waitForProcess, runProcess)
import System.Cmd (rawSystem)
import System.Directory (getCurrentDirectory, createDirectory, executable, getPermissions, setPermissions)
import System.FilePath ((</>))
import Data.List (isPrefixOf)
import Control.Monad

import Paths_virthualenv (getDataFileName)

getEnvVar :: String -> IO (Maybe String)
getEnvVar var = Just `fmap` getEnv var `catch` noValueHandler
    where noValueHandler e | isDoesNotExistError e = return Nothing
                           | otherwise             = ioError e

checkVHE :: IO ()
checkVHE = do
    x <- getEnvVar "VIRTHUALENV"
    case x of
        Nothing  -> return ()
        Just vhe -> do
            y <- getEnvVar "VIRTHUALENV_NAME"
            case y of
                Nothing -> error "VIRTHUALENV var is defined, but no VIRHTUALENV_NAME defined."
                Just vheName -> do
                    putStrLn $ "There is already active " ++ vheName ++ " Virtual Haskell Environment (at " ++ vhe ++ ")."
                    exitFailure

usage :: IO a
usage = do
    name <- getProgName
    putStrLn $ "usage: " ++ name ++ " ENV_NAME"
    putStrLn ""
    putStrLn "Creates Virtual Haskell Environment in the directory ENV_NAME"
    exitFailure

transplantPackage :: String -> String -> IO ()
transplantPackage ghcPackagePath package = do
  env <- getEnvironment
  let env' = ("GHC_PACKAGE_PATH", ghcPackagePath) : filter (\(k,v) -> k /= "GHC_PACKAGE_PATH") env
  (_, out, _, pid) <-
      runInteractiveProcess "ghc-pkg" ["describe", package] Nothing Nothing
  pid2 <- runProcess "ghc-pkg" ["register", "-"] Nothing (Just env') (Just out) Nothing Nothing
  mapM_ waitForProcess [pid, pid2]

subst :: (String, String) -> String -> String
subst _ [] = []
subst (from, to) input@(x:xs) | from `isPrefixOf` input = to ++ subst (from, to) (drop (length from) input)
                              | otherwise = x:subst (from, to) xs

sed :: [(String, String)] -> FilePath -> FilePath -> IO ()
sed substs inFile outFile = do
  inp <- readFile inFile
  let out = foldr subst inp substs
  writeFile outFile out

makeExecutable :: FilePath -> IO ()
makeExecutable f = do
  p <- getPermissions f
  setPermissions f (p {executable = True})

cabalUpdate :: FilePath -> FilePath -> IO ()
cabalUpdate ghcPackagePath cabalConfig = do
  env <- getEnvironment
  let env' = ("GHC_PACKAGE_PATH", ghcPackagePath) : filter (\(k,v) -> k /= "GHC_PACKAGE_PATH") env
  (_, _, _, pid) <-
      runInteractiveProcess "cabal"
                            ["--config-file=" ++ cabalConfig, "update"]
                            Nothing
                            (Just env')
  waitForProcess pid
  return ()

main :: IO ()
main = do
    checkVHE
    args <- getArgs
    virthualEnvName <- case args of
                        ["--help"] -> usage
                        ["-h"]     -> usage
                        [arg]      -> return arg
                        _          -> usage
    cabalConfigSkel  <- getDataFileName "cabal_config"
    cabalWrapperSkel <- getDataFileName "cabal"
    activateSkel     <- getDataFileName "activate"
    origCabalBinary  <- init `fmap` readProcess "which" ["cabal"] "" -- skip newline

    cwd <- getCurrentDirectory
    let virthualEnv    = cwd </> virthualEnvName
        virthualEnvDir = virthualEnv </> ".virthualenv"
        ghcPackagePath = virthualEnvDir </> "ghc_pkg_db"
        cabalDir = virthualEnvDir </> "cabal"
        cabalConfig = cabalDir </> "config"
        virthualEnvBinDir = virthualEnvDir </> "bin"
        activateScript = virthualEnvBinDir </> "activate"
        cabalWrapper = virthualEnvBinDir </> "cabal"
        bootPackages = [ "ffi", "rts", "ghc-prim", "integer-gmp", "base"
                       , "array", "containers", "filepath", "old-locale"
                       , "old-time", "unix", "directory", "pretty", "process"
                       , "Cabal", "bytestring", "ghc-binary"
                       , "bin-package-db", "hpc", "template-haskell", "ghc"
                       ]
    mapM_ createDirectory [virthualEnv, virthualEnvDir, cabalDir, virthualEnvBinDir]
    rawSystem "ghc-pkg" ["init", ghcPackagePath]
    mapM_ (transplantPackage ghcPackagePath) bootPackages
    sed [ ("<GHC_PACKAGE_PATH>", ghcPackagePath)
        , ("<CABAL_DIR>", cabalDir)
        ] cabalConfigSkel cabalConfig
    sed [ ("<VIRTHUALENV_NAME>", virthualEnvName)
        , ("<VIRTHUALENV>", virthualEnv)
        , ("<GHC_PACKAGE_PATH>", ghcPackagePath)
        ] activateSkel activateScript
    sed [ ("<ORIG_CABAL_BINARY>", origCabalBinary)
        , ("<CABAL_CONFIG>", cabalConfig)
        ] cabalWrapperSkel cabalWrapper
    makeExecutable cabalWrapper
    cabalUpdate ghcPackagePath cabalConfig
