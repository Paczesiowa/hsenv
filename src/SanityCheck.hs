module SanityCheck (sanityCheck) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Error (throwError)
import Control.Monad (when)
import System.Directory (doesDirectoryExist)

import Util.IO (getEnvVar)
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

-- check if everything is sane
sanityCheck :: MyMonad ()
sanityCheck = do
  checkVHE
  checkVirthualEnvAlreadyExists
