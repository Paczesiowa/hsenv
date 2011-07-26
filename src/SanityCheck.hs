module SanityCheck (sanityCheck) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Error (throwError)
import Control.Monad (when)
import System.Directory (doesDirectoryExist)
import Control.Monad.Reader (asks)

import Util.IO (getEnvVar, which)
import Types
import MyMonad
import Paths (vheDirStructure)

-- check if any virtual env is already active
checkVHE :: MyMonad ()
checkVHE = do
    virthualEnvVar <- liftIO $ getEnvVar "VIRTHUALENV"
    case virthualEnvVar of
        Nothing   -> return ()
        Just path -> do
            virthualEnvName <- liftIO $ getEnvVar "VIRTHUALENV_NAME"
            case virthualEnvName of
                Nothing -> do
                       debug $ "warning: VIRTHUALENV environment variable is defined" ++ ", but no VIRHTUALENV_NAME environment variable defined."
                       throwError $ MyException $ "There is already active Virtual Haskell Environment (at " ++ path ++ ")."
                Just name ->
                    throwError $ MyException $ "There is already active " ++ name ++ " Virtual Haskell Environment (at " ++ path ++ ")."

checkVirthualEnvAlreadyExists :: MyMonad ()
checkVirthualEnvAlreadyExists = do
  dirStructure <- vheDirStructure
  flag <- liftIO $ doesDirectoryExist $ virthualEnvDir dirStructure
  when flag $ throwError $ MyException $ "There is already .virthualenv directory at " ++ virthualEnv dirStructure

-- check if cabal binary exist on PATH
checkCabalInstall :: MyMonad ()
checkCabalInstall = do
  cabalInstallPath <- liftIO $ which "cabal"
  case cabalInstallPath of
    Just _  -> return ()
    Nothing -> throwError $ MyException "Couldn't find cabal binary (from cabal-install package) in your $PATH."

-- check if GHC tools (ghc, ghc-pkg) exist on PATH
-- skip the check if using GHC from a tarball
checkGhc :: MyMonad ()
checkGhc = do
  ghcSrc <- asks ghcSource
  case ghcSrc of
    Tarball _ -> return ()
    System    -> do
      ghcPath <- liftIO $ which "ghc"
      case ghcPath of
        Just _  -> return ()
        Nothing -> throwError $ MyException "Couldn't find ghc binary in your $PATH."
      ghc_pkgPath <- liftIO $ which "ghc-pkg"
      case ghc_pkgPath of
        Just _  -> return ()
        Nothing -> throwError $ MyException "Couldn't find ghc-pkg binary in your $PATH."

-- check if everything is sane
sanityCheck :: MyMonad ()
sanityCheck = do
  checkVHE
  checkVirthualEnvAlreadyExists
  checkCabalInstall
  checkGhc
