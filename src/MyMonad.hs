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
import Control.Monad.Writer (WriterT, MonadWriter, runWriterT, tell)
import Control.Monad.State (StateT, MonadState, evalStateT, modify, gets)
import Control.Monad.Error (ErrorT, MonadError, runErrorT)
import Control.Monad (when)

import Prelude hiding (log)

newtype MyMonad a = MyMonad (StateT MyState (ReaderT Options (ErrorT MyException (WriterT [String] IO))) a)
    deriving (Monad, MonadReader Options, MonadIO, MonadState MyState, MonadError MyException, MonadWriter [String])

runMyMonad :: MyMonad a -> Options -> IO (Either MyException a, [String])
runMyMonad (MyMonad m) = runWriterT . runErrorT . runReaderT (evalStateT m (MyState 0))

indentMessages :: MyMonad a -> MyMonad a
indentMessages m = do
  modify (\s -> s{logDepth = logDepth s + 2})
  result <- m
  modify (\s -> s{logDepth = logDepth s - 2})
  return result

log :: Verbosity -> String -> MyMonad ()
log minLevel str = do
  depth <- gets logDepth
  let text = replicate (fromInteger depth) ' ' ++ str
  tell [text]
  flag <- asks verbosity
  when (flag >= minLevel) $ do
    liftIO $ putStrLn text

debug :: String -> MyMonad ()
debug = log Verbose

info :: String -> MyMonad ()
info = log Quiet

trace :: String -> MyMonad ()
trace = log VeryVerbose
