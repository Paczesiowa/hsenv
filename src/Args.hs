{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Args ( usage
            , parseArgs
            ) where

import System.Directory (getCurrentDirectory)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Data.Monoid (Monoid(..))
import System.Environment (getProgName)
import System.FilePath (splitPath)
import Control.Monad.Error (MonadError, ErrorT, runErrorT, throwError)
import Control.Monad.State (MonadState, StateT, runStateT, get, put)
import Control.Monad.Trans (MonadIO, liftIO)

import Types

usage :: IO ()
usage = do
    name <- getProgName
    putStrLn $ "usage: " ++ name ++ " [FLAGS]"
    putStrLn ""
    putStrLn "Flags:"
    putStrLn "-h --help           Show this help message"
    putStrLn "--version           Print version number"
    putStrLn "--verbose           Print some debugging info"
    putStrLn "--very-verbose      Print some debugging info"
    putStrLn "--skip-sanity-check Skip all the sanity checks (use at your own risk)"
    putStrLn "--name=NAME         Use NAME for name of Virthual Haskell Environment"
    putStrLn "                    (defaults to the name of the current directory)"
    putStrLn "--ghc=FILE          Use GHC from provided tarball (e.g. ghc-7.0.4-i386-unknown-linux.tar.bz2)"
    putStrLn "                    Without this flag virthualenv will use system's copy of GHC"
    putStrLn "--make-cmd=NAME     Used as make substitute for installing GHC from tarball (e.g. gmake),"
    putStrLn "                    defaults to 'make'"
    putStrLn ""
    putStrLn "Creates Virtual Haskell Environment in the current directory."
    putStrLn "All files will be stored in the .virthualenv/ subdirectory."

newtype ArgMonad a = ArgMonad (ErrorT String (StateT Args IO) a)
    deriving (MonadState Args, MonadError String, Monad, MonadIO)

data Args = Args { shortArgs      :: [Char]
                 , longArgs       :: [String]
                 , longValArgs    :: [(String, String)]
                 , positionalArgs :: [String]
                 }
    deriving Show

emptyArgs :: Args
emptyArgs = Args [] [] [] []

instance Monoid Args where
    mempty = Args [] [] [] []
    Args x1 y1 z1 t1 `mappend` Args x2 y2 z2 t2 = Args (x1 ++ x2) (y1 ++ y2) (z1 ++ z2) (t1 ++ t2)

runArgMonad :: ArgMonad a -> [String] -> IO (Either String a)
runArgMonad (ArgMonad m) args =
    case parseArguments args of
      Left err         -> return $ Left err
      Right parsedArgs -> do
        (result, leftOverArgs) <- runStateT (runErrorT m) parsedArgs
        case leftOverArgs of
          Args [] [] [] [] -> return result
          Args shortOptions _ _ _ | not (null shortOptions) ->
            return $ Left $ "Unknown short option identifiers: " ++ shortOptions
          Args _ longOptions _ _  | not (null longOptions) ->
            return $ Left $ "Unknown long option identifiers: " ++ unwords longOptions
          Args _ _ longKeyValOptions _ | not (null longKeyValOptions) ->
            return $ Left $ "Unknown long key-value options: " ++ unwords (map (\(k,v) -> k++"="++v) longKeyValOptions)
          Args _ _ _ _ -> return $ Left $ "Unknown positional options: " ++ unwords (positionalArgs leftOverArgs)

parseArguments :: [String] -> Either String Args
parseArguments args =
    case break (== "--") args of
      (keywordArgs, "--":rest) -> do
        parsedKeywordArgs <- mapM parseArgument keywordArgs
        return $ mconcat parsedKeywordArgs `mappend` emptyArgs{positionalArgs = rest}

      (_, []) -> mconcat `fmap` mapM parseArgument args
      _ -> error "I'm stupid and I cannot code. I'm sorry."

parseArgument :: String -> Either String Args
parseArgument arg | "--" `isPrefixOf` arg
                  && "="  `isInfixOf` arg =
                      let (x, y) = break (=='=') arg
                          key = drop (length "--") x
                          val = drop (length "=")  y
                      in return emptyArgs{longValArgs = [(key, val)]}
                  | "--" `isPrefixOf` arg =
                      return emptyArgs{longArgs = [drop (length "--") arg]}
                  | "-" `isPrefixOf` arg =
                      let symbols = tail arg
                      in case symbols of
                           [] -> throwError "Empty list of short options (after '-')"
                           _ | any (not . isAlphaNum) symbols -> throwError "Non alpha-numeric short option"
                             | otherwise -> return emptyArgs{shortArgs = symbols}
                  | otherwise = return emptyArgs{positionalArgs = [arg]}

getSingleLongValueArg :: String -> ArgMonad (Maybe String)
getSingleLongValueArg argName = do
  args <- get
  case filter (\(k,_) -> k == argName) $ longValArgs args of
    [] -> return Nothing
    [(_, val)] -> do
      put args{longValArgs = filter (\(k,_) -> k /= argName) $ longValArgs args}
      return $ Just val
    _ -> throwError $ "Multiple values with key " ++ argName

isSingleValueSet :: String -> ArgMonad Bool
isSingleValueSet argName = do
  args <- get
  case filter (==argName) $ longArgs args of
    [] -> return False
    [_] -> do
      put args{longArgs = filter (/=argName) $ longArgs args}
      return True
    _ -> throwError $ "Multiple values named " ++ argName

realParseArgs :: ArgMonad Options
realParseArgs = do
  verbosityFlag  <- isSingleValueSet "verbose"
  verbosityFlag2 <- isSingleValueSet "very-verbose"
  let verboseness = case (verbosityFlag, verbosityFlag2) of
                      (_, True)      -> VeryVerbose
                      (True, False)  -> Verbose
                      (False, False) -> Quiet
  nameFlag <- getSingleLongValueArg "name"
  name <- case nameFlag of
           Just name' -> return name'
           Nothing    -> do
             cwd <- liftIO getCurrentDirectory
             let dirs = splitPath cwd
                 name = last dirs
             when (verboseness > Quiet) $ liftIO $ putStrLn $ "Using current directory name as Virtual Haskell Environment name: " ++ name
             return name
  ghcFlag <- getSingleLongValueArg "ghc"
  let ghc = case ghcFlag of
              Nothing   -> System
              Just path -> Tarball path
  skipSanityCheckFlag <- isSingleValueSet "skip-sanity-check"
  makeCmdFlag <- getSingleLongValueArg "make-cmd"
  let make = fromMaybe "make" makeCmdFlag
  return Options{ verbosity = verboseness
                , skipSanityCheck = skipSanityCheckFlag
                , vheName   = name
                , ghcSource = ghc
                , makeCmd   = make
                }

parseArgs :: [String] -> IO (Either String Options)
parseArgs = runArgMonad realParseArgs
