import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import System.FilePath ((</>))

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
                (result, messageLog) <- runMyMonad realMain options
                case result of
                  Left err -> do
                    hPutStrLn stderr $ getExceptionMessage err
                    hPutStrLn stderr ""
                    hPutStrLn stderr "virthualenv.log file contains detailed description of the process."
                    let errorLog = unlines $ messageLog ++ ["", getExceptionMessage err]
                    writeFile "virthualenv.log" errorLog
                    exitFailure
                  Right ()  -> writeFile (".virthualenv" </> "virthualenv.log") $ unlines messageLog

realMain :: MyMonad ()
realMain = do
  skipSanityCheckFlag <- asks skipSanityCheck
  if skipSanityCheckFlag then
      info "WARNING: sanity checks are disabled."
   else
      sanityCheck
  createDirStructure
  installGhc
  initGhcDb
  copyBaseSystem
  installCabalConfig
  installActivateScript
  installCabalWrapper
  cabalUpdate
