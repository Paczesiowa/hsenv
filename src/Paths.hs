module Paths ( hseDirStructure
             , cabalConfigLocation
             , getVirtualEnvironment
             , dotDirName
             ) where

import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment)

import Types
import MyMonad

-- returns record containing paths to all important directories
-- inside virtual environment dir structure
hseDirStructure :: MyMonad DirStructure
hseDirStructure = do
  cwd <- liftIO getCurrentDirectory
  dirName <- dotDirName
  let hsEnvLocation    = cwd
      hsEnvDirLocation = hsEnvLocation </> dirName
      cabalDirLocation = hsEnvDirLocation </> "cabal"
      ghcDirLocation   = hsEnvDirLocation </> "ghc"
  return DirStructure { hsEnv          = hsEnvLocation
                      , hsEnvDir       = hsEnvDirLocation
                      , ghcPackagePath = hsEnvDirLocation </> "ghc_pkg_db"
                      , cabalDir       = cabalDirLocation
                      , cabalBinDir    = cabalDirLocation </> "bin"
                      , hsEnvBinDir    = hsEnvDirLocation </> "bin"
                      , ghcDir         = ghcDirLocation
                      , ghcBinDir      = ghcDirLocation </> "bin"
                      }

-- directory name of hsEnvDir
dotDirName :: MyMonad String
dotDirName = do
  name <- asks hsEnvName
  return $ ".hsenv_" ++ name

-- returns location of cabal's config file inside virtual environment dir structure
cabalConfigLocation :: MyMonad FilePath
cabalConfigLocation = do
  dirStructure <- hseDirStructure
  return $ cabalDir dirStructure </> "config"

-- returns environment dictionary used in Virtual Haskell Environment
-- it's inherited from the current process, but variable
-- GHC_PACKAGE_PATH is altered.
getVirtualEnvironment :: MyMonad [(String, String)]
getVirtualEnvironment = do
  env <- liftIO getEnvironment
  dirStructure <- hseDirStructure
  return $ ("GHC_PACKAGE_PATH", ghcPackagePath dirStructure) : filter (\(k,_) -> k /= "GHC_PACKAGE_PATH") env
