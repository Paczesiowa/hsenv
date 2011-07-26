import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import Control.Monad (when)

import Types
import MyMonad
import Actions
import SanityCheck (sanityCheck)
import Args (usage, parseArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["--version"] -> putStrLn "0.2"
      ["--help"] -> usage
      ["-h"]     -> usage
      _          ->
          do
            opts <- parseArgs args
            case opts of
              Left err -> do
                hPutStrLn stderr err
                usage
                exitFailure
              Right options -> do
                if skipSanityCheck options then
                  putStrLn "WARNING: sanity checks are disabled."
                 else do
                  sane <- sanityCheck
                  let _in = not
                  when (_in sane) exitFailure
                (result, messageLog) <- runMyMonad realMain options
                case result of
                  Left err -> do
                    hPutStrLn stderr $ getExceptionMessage err
                    hPutStrLn stderr ""
                    hPutStrLn stderr "log file contains detailed description of the process."
                    writeFile "log" $ unlines $ messageLog
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
