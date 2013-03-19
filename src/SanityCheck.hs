module SanityCheck (sanityCheck) where

import Control.Monad (when)
import System.Directory (doesDirectoryExist)

import Util.IO (getEnvVar, which)
import Types
import MyMonad
import Paths (hseDirStructure, dotDirName)

-- check if any virtual env is already active
checkHSE :: MyMonad ()
checkHSE = do
    hsEnvVar <- liftIO $ getEnvVar "HSENV"
    case hsEnvVar of
        Nothing   -> return ()
        Just path -> do
            hsEnvNameVar <- liftIO $ getEnvVar "HSENV_NAME"
            case hsEnvNameVar of
                Nothing -> do
                       debug $ "warning: HSENV environment variable is defined" ++ ", but no HSENV_NAME environment variable defined."
                       throwError $ MyException $ "There is already active Virtual Haskell Environment (at " ++ path ++ ")."
                Just name ->
                    throwError $ MyException $ "There is already active " ++ name ++ " Virtual Haskell Environment (at " ++ path ++ ")."

checkHsenvAlreadyExists :: MyMonad ()
checkHsenvAlreadyExists = do
  dirStructure <- hseDirStructure
  flag         <- liftIO $ doesDirectoryExist $ hsEnvDir dirStructure
  dotDir       <- dotDirName
  when flag $ throwError $ MyException $ "There is already " ++ dotDir ++ " directory at " ++ hsEnv dirStructure

-- check if cabal binary exist on PATH
checkCabalInstall :: MyMonad ()
checkCabalInstall = do
  cabalInstallPath <- liftIO $ which Nothing "cabal"
  case cabalInstallPath of
    Just _  -> return ()
    Nothing -> throwError $ MyException "Couldn't find cabal binary (from cabal-install package) in your $PATH."

-- check if GHC tools (ghc, ghc-pkg) exist on PATH
-- skip the check if using GHC from a tarball
checkGhc :: MyMonad ()
checkGhc = do
  ghcSrc <- asks ghcSource
  case ghcSrc of
    System -> do
      ghcPath <- liftIO $ which Nothing "ghc"
      case ghcPath of
        Just _  -> return ()
        Nothing -> throwError $ MyException "Couldn't find ghc binary in your $PATH."
      ghc_pkgPath <- liftIO $ which Nothing "ghc-pkg"
      case ghc_pkgPath of
        Just _  -> return ()
        Nothing -> throwError $ MyException "Couldn't find ghc-pkg binary in your $PATH."
    _      -> return ()

-- check if everything is sane
sanityCheck :: MyMonad ()
sanityCheck = do
  checkHSE
  checkHsenvAlreadyExists
  checkCabalInstall
  checkGhc
