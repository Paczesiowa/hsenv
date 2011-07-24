import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import System.Directory (removeDirectoryRecursive)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Codec.Archive.Tar as Tar
import Codec.Compression.BZip
import qualified Data.ByteString.Lazy as BS

import Types
import MyMonad
import Paths
import Actions
import SanityCheck (sanityCheck)
import Args (usage, parseArgs)

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
