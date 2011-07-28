module Paths ( vheDirStructure
             , cabalConfigLocation
             , getVirtualEnvironment
             ) where

import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment)

import Types
import MyMonad

-- returns record containing paths to all important directories
-- inside virtual environment dir structure
vheDirStructure :: MyMonad DirStructure
vheDirStructure = do
  cwd <- liftIO getCurrentDirectory
  let virthualEnvLocation    = cwd
      virthualEnvDirLocation = virthualEnvLocation </> ".virthualenv"
      cabalDirLocation       = virthualEnvDirLocation </> "cabal"
      ghcDirLocation         = virthualEnvDirLocation </> "ghc"
  return DirStructure { virthualEnv       = virthualEnvLocation
                      , virthualEnvDir    = virthualEnvDirLocation
                      , ghcPackagePath    = virthualEnvDirLocation </> "ghc_pkg_db"
                      , cabalDir          = cabalDirLocation
                      , cabalBinDir       = cabalDirLocation </> "bin"
                      , virthualEnvBinDir = virthualEnvDirLocation </> "bin"
                      , ghcDir            = ghcDirLocation
                      , ghcBinDir         = ghcDirLocation </> "bin"
                      }

-- returns location of cabal's config file inside virtual environment dir structure
cabalConfigLocation :: MyMonad FilePath
cabalConfigLocation = do
  dirStructure <- vheDirStructure
  return $ cabalDir dirStructure </> "config"

-- returns environment dictionary used in Virtual Haskell Environment
-- it's inherited from the current process, but variable
-- GHC_PACKAGE_PATH is altered.
getVirtualEnvironment :: MyMonad [(String, String)]
getVirtualEnvironment = do
  env <- liftIO getEnvironment
  dirStructure <- vheDirStructure
  return $ ("GHC_PACKAGE_PATH", ghcPackagePath dirStructure) : filter (\(k,_) -> k /= "GHC_PACKAGE_PATH") env
