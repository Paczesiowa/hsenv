module Process ( externalGhcPkgDb
               , outsideGhcPkg
               , insideGhcPkg
               ) where

import Types
import MyMonad
import Paths

import Util.IO (readProcessWithExitCodeInEnv)

import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode)

-- run outside ghc-pkg tool (uses system's or from ghc installed from tarball)
outsideGhcPkg :: [String] -> MyMonad (ExitCode, String, String)
outsideGhcPkg args = do
  ghc <- asks ghcSource
  dirStructure <- vheDirStructure
  let ghcPkg = case ghc of
                 System    -> "ghc-pkg"
                 Tarball _ -> ghcDir dirStructure </> "bin" </> "ghc-pkg"
  liftIO $ readProcessWithExitCode ghcPkg args ""

-- returns path to GHC (installed from tarball) builtin package database
externalGhcPkgDb :: MyMonad FilePath
externalGhcPkgDb = do
  (_, out, _) <- outsideGhcPkg ["list"]
  let lineWithPath = head $ lines out
      path         = init lineWithPath -- skip trailing colon
  return path

-- run ghc-pkg tool (uses system's or from ghc installed from tarball)
-- from the inside of Virtual Haskell Environment
insideGhcPkg :: [String] -> Maybe String -> MyMonad (ExitCode, String, String)
insideGhcPkg args input = do
  ghc <- asks ghcSource
  dirStructure <- vheDirStructure
  env <- getVirtualEnvironment
  let ghcPkg = case ghc of
                 System    -> "ghc-pkg"
                 Tarball _ -> ghcDir dirStructure </> "bin" </> "ghc-pkg"
  liftIO $ readProcessWithExitCodeInEnv env ghcPkg args input
