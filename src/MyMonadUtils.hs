module MyMonadUtils (runInTmpDir) where

import System.Directory
import Util.IO
import MyMonad

runInTmpDir :: MyMonad a -> MyMonad a
runInTmpDir m = do
  tmp <- liftIO getTemporaryDirectory
  tmpDir <- liftIO $ createTemporaryDirectory tmp "hsenv"
  oldCwd <- liftIO getCurrentDirectory
  liftIO $ setCurrentDirectory tmpDir
  let cleanup = do
        liftIO $ setCurrentDirectory oldCwd
        liftIO $ removeDirectoryRecursive tmpDir
  m `finally` cleanup
