import System.Environment (getProgName, getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import System.Directory (getCurrentDirectory, removeDirectoryRecursive)
import System.FilePath (splitPath)
import Data.List (isPrefixOf)
import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Codec.Archive.Tar as Tar
import Codec.Compression.BZip
import qualified Data.ByteString.Lazy as BS

import Types
import MyMonad
import Paths
import Actions
import SanityCheck (sanityCheck)

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

main :: IO ()
main = do
    sane <- sanityCheck
    let _in = not
    when (_in sane) exitFailure

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
