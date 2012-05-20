{-# LANGUAGE Arrows #-}

module Args (getArgs) where

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
ghcInfo = "Use GHC from provided tarball (e.g. ghc-7.0.4-i386-unknown-linux.tar.bz2)"
sanityInfo = "Skip all the sanity checks (use at your own risk)"
makeInfo = "Used as make substitute for installing GHC from tarball (e.g. gmake)"

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
  ghcFlag <- getOptionWithDefault2 "ghc" "FILE" "system's copy of GHC" ghcInfo -< ()
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

getArgs :: IO Options
getArgs = parseArgs realParseArgs "0.2" outro

outro = "Creates Virtual Haskell Environment in the current directory.\n"
        ++ "All files will be stored in the .hsenv/ subdirectory."
