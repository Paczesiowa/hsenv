module Process ( externalGhcPkgDb
               , outsideGhcPkg
               , insideGhcPkg
               , runProcess
               ) where

import Types
import MyMonad
import Paths

import Util.IO (readProcessWithExitCodeInEnv, Environment)

import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error (throwError)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

runProcess :: Maybe Environment -> FilePath -> [String] -> Maybe String -> MyMonad String
runProcess env prog args input = do
  debug $ unwords $ ["Executing:", prog] ++ args
  indentMessages $ case env of
    Nothing   -> trace "using inherited variable environment"
    Just env' -> do
      trace "using following environment:"
      indentMessages $ forM_ env' $ \(var,val) -> trace $ var ++ ": " ++ val
  indentMessages $ case input of
    Nothing  -> return ()
    Just inp -> do
      trace "using the following input:"
      indentMessages $ forM_ (lines inp) trace
  let execProcess = case env of
                      Nothing   -> readProcessWithExitCode prog args (fromMaybe "" input)
                      Just env' -> readProcessWithExitCodeInEnv env' prog args input
  (exitCode, output, errors) <- liftIO execProcess
  indentMessages $ debug $ case exitCode of
    ExitSuccess         -> "Process exited successfully"
    ExitFailure errCode -> "Process failed with exit code " ++ show errCode
  indentMessages $ do
    trace "Process output:"
    indentMessages $ forM_ (lines output) trace
  indentMessages $ do
    trace "Process error output:"
    indentMessages $ forM_ (lines errors) trace
  case exitCode of
    ExitSuccess         -> return output
    ExitFailure errCode -> throwError $ MyException $ prog ++ " process failed with status " ++ show errCode

-- run outside ghc-pkg tool (uses system's or from ghc installed from tarball)
outsideGhcPkg :: [String] -> MyMonad String
outsideGhcPkg args = do
  ghc <- asks ghcSource
  dirStructure <- vheDirStructure
  ghcPkg <- case ghc of
    System    -> do
      debug "Running system's version of ghc-pkg"
      return "ghc-pkg"
    Tarball _ -> do
      debug "Running ghc-pkg installed from GHC's tarball"
      return $ ghcDir dirStructure </> "bin" </> "ghc-pkg"
  indentMessages $ runProcess Nothing ghcPkg args Nothing

-- returns path to GHC (installed from tarball) builtin package database
externalGhcPkgDb :: MyMonad FilePath
externalGhcPkgDb = do
  debug "Checking where GHC (installed from tarball) keeps its package database"
  out <- indentMessages $ outsideGhcPkg ["list"]
  indentMessages $ debug "Trying to parse ghc-pkg's output"
  case lines out of
    []             -> throwError $ MyException "ghc-pkg returned empty output"
    lineWithPath:_ ->
      case lineWithPath of
        "" -> throwError $ MyException "ghc-pkg's first line of output is empty"
        _  -> do
          indentMessages $ debug $ "Found: " ++ lineWithPath
          return lineWithPath

-- run ghc-pkg tool (uses system's or from ghc installed from tarball)
-- from the inside of Virtual Haskell Environment
insideGhcPkg :: [String] -> Maybe String -> MyMonad String
insideGhcPkg args input = do
  ghc <- asks ghcSource
  dirStructure <- vheDirStructure
  env <- getVirtualEnvironment
  ghcPkg <- case ghc of
    System    -> do
      debug "Running system's version of ghc-pkg inside virtual environment"
      return "ghc-pkg"
    Tarball _ -> do
      debug "Running ghc-pkg, installed from GHC's tarball, inside virtual environment"
      return $ ghcDir dirStructure </> "bin" </> "ghc-pkg"
  indentMessages $ runProcess (Just env) ghcPkg args input
