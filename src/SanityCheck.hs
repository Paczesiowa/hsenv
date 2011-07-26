module SanityCheck (sanityCheck) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Error (throwError)

import Util.IO (getEnvVar)
import Types
import MyMonad

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

-- check if everything is sane
sanityCheck :: MyMonad ()
sanityCheck = do
  checkVHE
  return ()
