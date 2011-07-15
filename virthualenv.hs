import System.Environment (getEnv, getProgName, getArgs, getEnvironment)
import System.IO (stderr, hPutStrLn)
import System.IO.Error (isDoesNotExistError)
import System.Exit (exitFailure, ExitCode(..))
import System.Process (readProcess, runInteractiveProcess, waitForProcess, runProcess)
import System.Cmd (rawSystem)
import System.Directory (getCurrentDirectory, createDirectory, executable, getPermissions, setPermissions)
import System.FilePath ((</>))
import Data.List (isPrefixOf, intercalate)
import Control.Monad
import Data.Char (isSpace)
import Distribution.Compat.ReadP
import Distribution.Package
import Distribution.Version
import Distribution.Text
import Data.Maybe(catMaybes)

import Paths_virthualenv (getDataFileName)

getEnvVar :: String -> IO (Maybe String)
getEnvVar var = Just `fmap` getEnv var `catch` noValueHandler
    where noValueHandler e | isDoesNotExistError e = return Nothing
                           | otherwise             = ioError e

-- check if any virtual env is already active
checkVHE :: IO Bool
checkVHE = do
    virthualEnv <- getEnvVar "VIRTHUALENV"
    case virthualEnv of
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
    putStrLn $ "usage: " ++ name ++ " ENV_NAME"
    putStrLn ""
    putStrLn "Creates Virtual Haskell Environment in the directory ENV_NAME"

-- TODO: it should return IO (Maybe String)
-- TODO: it should walk the PATH elems, instead of using system's which util
which :: String -> IO String
which progName = do
  output <- readProcess "which" [progName] ""
  let result = init output -- skip final newline
  return result

prettyPkgInfo :: PackageIdentifier -> String
prettyPkgInfo (PackageIdentifier (PackageName pkgName) (Version [] _)) = pkgName
prettyPkgInfo (PackageIdentifier (PackageName pkgName) (Version numbers _)) =
  pkgName ++ "-" ++ intercalate "." (map show numbers)

getDeps :: PackageIdentifier -> IO [PackageIdentifier]
getDeps pkgInfo = do
  x <- readProcess "ghc-pkg" ["field", prettyPkgInfo pkgInfo, "depends"] ""
  let trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
      depStrings = concat $ map tail $ map words $ map trim $ lines x
  mapM parsePackageName depStrings

transplantPackage :: String -> String -> IO ()
transplantPackage ghcPackagePath package = do
  -- check if package has version attached
  print $ "transplanting " ++  package
  print "dupa"
  print package
  pkgInfo <- parsePackageName package
  print "po dupie"
  pkgInfo' <- case versionBranch $ pkgVersion pkgInfo of
                 [] -> do
                   out <- readProcess "ghc-pkg" ["field", package, "version"] ""
                   let versionStrings = map (!!1) $ map words $ lines out
                       versions = catMaybes $ map (\s -> parseCheck parse s "version") versionStrings
                       version = minimum versions
                   return pkgInfo{pkgVersion = version}
                 _ -> return pkgInfo
  env <- getEnvironment
  let env' = ("GHC_PACKAGE_PATH", ghcPackagePath) : filter (\(k,v) -> k /= "GHC_PACKAGE_PATH") env
  pid <- runProcess "ghc-pkg" ["describe", package] Nothing (Just env') Nothing Nothing Nothing
  exitCode <- waitForProcess pid
  case exitCode of
    ExitSuccess -> print "transplanted" --return ()
    _ -> do
      deps <- getDeps pkgInfo'
      print deps
      mapM (transplantPackage ghcPackagePath) $ map prettyPkgInfo deps
      movePackage ghcPackagePath pkgInfo'


-- parseCheck :: ReadP a a -> String -> String -> IO a
parseCheck parser str what =
  case [ x | (x,ys) <- readP_to_S parser str, all isSpace ys ] of
    [x] -> return x
    _ -> error ("cannot parse \'" ++ str ++ "\' as a " ++ what)

parsePackageName :: String -> IO PackageIdentifier
parsePackageName str | "builtin_" `isPrefixOf` str =
                         let pkgName = drop (length "builtin_") str
                         in return $ PackageIdentifier (PackageName pkgName) $ Version [] []
                     | otherwise = parseCheck parse str "package identifier"

-- copy single package that already has all deps satisfied
movePackage :: String -> PackageIdentifier -> IO ()
movePackage ghcPackagePath pkgInfo = do
  env <- getEnvironment
  let env' = ("GHC_PACKAGE_PATH", ghcPackagePath) : filter (\(k,v) -> k /= "GHC_PACKAGE_PATH") env
      package = prettyPkgInfo pkgInfo
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
  print inFile
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
    envActive <- checkVHE
    when envActive exitFailure

    args <- getArgs
    case args of
      ["--help"] -> usage
      ["-h"]     -> usage
      [arg]      -> realMain arg
      _          -> usage >> exitFailure

realMain :: String -> IO ()
realMain virthualEnvName = do
    cabalConfigSkel  <- getDataFileName "cabal_config"
    cabalWrapperSkel <- getDataFileName "cabal"
    activateSkel     <- getDataFileName "activate"
    origCabalBinary  <- which "cabal"

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
    transplantPackage ghcPackagePath "base"
    -- mapM_ (transplantPackage ghcPackagePath) bootPackages
    print cabalConfigSkel
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
