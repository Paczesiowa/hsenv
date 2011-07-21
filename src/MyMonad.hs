{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MyMonad ( MyMonad
               , runMyMonad
               , indentMessages
               , debug
               , info
               , trace
               ) where

import Types

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
import Control.Monad.State (StateT, MonadState, evalStateT, modify, gets)
import Control.Monad.Error (ErrorT, MonadError, runErrorT)  

newtype MyMonad a = MyMonad (StateT MyState (ReaderT Options (ErrorT MyException IO)) a)
    deriving (Monad, MonadReader Options, MonadIO, MonadState MyState, MonadError MyException)

runMyMonad :: MyMonad a -> Options -> IO (Either MyException a)
runMyMonad (MyMonad m) = runErrorT . runReaderT (evalStateT m (MyState 0))

indentMessages :: MyMonad a -> MyMonad a
indentMessages m = do
  modify (\s -> s{logDepth = logDepth s + 2})
  result <- m
  modify (\s -> s{logDepth = logDepth s - 2})
  return result

debug :: String -> MyMonad ()
debug s = do
  flag <- asks verbose
  if flag then do
      depth <- gets logDepth
      liftIO $ putStrLn $ replicate (fromInteger depth) ' ' ++ s
   else
      return ()

info :: String -> MyMonad ()
info = debug

trace :: String -> MyMonad ()
trace = debug
