{-# LANGUAGE Arrows #-}

module Args ( usage
            , parseArgs
            ) where

import Control.Arrow
import Util.Args hiding (usage)
import qualified Util.Args
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
-- import Control.Monad.Trans (MonadIO, liftIO)

import Types

verboseInfo = "Print some debugging info"
veryVerboseInfo = "Print some more debugging info"
nameInfo = "Use NAME as name of the Virtual Haskell Environment"
ghcInfo = ""
sanityInfo = ""
makeInfo = ""

realParseArgs :: ArgArrow () Options
realParseArgs = proc () -> do
  verbosityFlag  <- getSwitch "verbose" verboseInfo -< ()
  verbosityFlag2 <- getSwitch "very-verbose" veryVerboseInfo -< ()
  let verboseness = case (verbosityFlag, verbosityFlag2) of
                      (_, True)      -> VeryVerbose
                      (True, False)  -> Verbose
                      (False, False) -> Quiet
  nameFlag <- getOptionWithDefault2 "name" "NAME" "current directory name" nameInfo -< ()
  name <- case nameFlag of
           Just name' -> returnA -< name'
           Nothing -> do
             cwd <- liftIO' getCurrentDirectory -< ()
             returnA -< last $ splitPath cwd
  ghcFlag <- getOptionWithDefault2 "ghc" "FILE" "" ghcInfo -< ()
  let ghc = case ghcFlag of
              Nothing   -> System
              Just path -> Tarball path
  skipSanityCheckFlag <- getSwitch "skip-sanity-check" sanityInfo -< ()
  make <- getOptionWithDefault "make-cmd" "NAME" "make" makeInfo -< ()
  returnA -< Options{ verbosity       = verboseness
                   , skipSanityCheck = skipSanityCheckFlag
                   , hsEnvName       = name
                   , ghcSource       = ghc
                   , makeCmd         = make
                   }

parseArgs :: [String] -> IO Options
parseArgs args = runArgArrow (parseArguments args) realParseArgs

usage = Util.Args.usage realParseArgs >>= putStrLn
