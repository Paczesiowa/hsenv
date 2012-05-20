{-# LANGUAGE Arrows #-}

module Args (getArgs) where

import Control.Arrow
import Util.Args
import System.Directory (getCurrentDirectory)
import System.FilePath (splitPath)
import Types
import Paths_hsenv (version)

verbosityOpt, veryVerbosityOpt, skipSanityOpt :: SwitchDescription
verbosityOpt = SwitchDescription
               { switchName = "verbose"
               , switchHelp = "Print some debugging info"
               }
veryVerbosityOpt = SwitchDescription
                   { switchName = "very-verbose"
                   , switchHelp = "Print some more debugging info"
                   }
skipSanityOpt = SwitchDescription
                { switchName = "skip-sanity-check"
                , switchHelp = "Skip all the sanity checks (use at your own risk)"
                }

nameOpt, ghcOpt :: DynamicOptionDescription
nameOpt = DynamicOptionDescription
          { dynamicOptionName        = "name"
          , dynamicOptionTemplate    = "NAME"
          , dynamicOptionDescription = "current directory name"
          , dynamicOptionHelp = "Use NAME as name of the Virtual Haskell Environment"
          }
ghcOpt = DynamicOptionDescription
         { dynamicOptionName        = "ghc"
         , dynamicOptionTemplate    = "FILE"
         , dynamicOptionDescription = "system's copy of GHC"
         , dynamicOptionHelp =
             "Use GHC from provided tarball (e.g. ghc-7.0.4-i386-unknown-linux.tar.bz2)"
         }

makeOpt :: StaticOptionDescription
makeOpt = StaticOptionDescription
          { staticOptionName = "make-cmd"
          , staticOptionTemplate = "CMD"
          , staticOptionDefault = "make"
          , staticOptionHelp =
              "Used as make substitute for installing GHC from tarball (e.g. gmake)"
          }

realParseArgs :: ArgArrow () Options
realParseArgs = proc () -> do
  verbosityFlag <- get verbosityOpt -< ()
  verbosityFlag2 <- get veryVerbosityOpt -< ()
  let verboseness = case (verbosityFlag, verbosityFlag2) of
                      (_, True)      -> VeryVerbose
                      (True, False)  -> Verbose
                      (False, False) -> Quiet
  nameFlag <- get nameOpt -< ()
  name <- case nameFlag of
           Just name' -> returnA -< name'
           Nothing -> do
             cwd <- liftIO' getCurrentDirectory -< ()
             returnA -< last $ splitPath cwd
  ghcFlag <- get ghcOpt -< ()
  let ghc = case ghcFlag of
              Nothing   -> System
              Just path -> Tarball path
  skipSanityCheckFlag <- get skipSanityOpt -< ()
  make <- get makeOpt -< ()
  returnA -< Options{ verbosity       = verboseness
                   , skipSanityCheck = skipSanityCheckFlag
                   , hsEnvName       = name
                   , ghcSource       = ghc
                   , makeCmd         = make
                   }

getArgs :: IO Options
getArgs = parseArgs realParseArgs version outro
    where outro = "Creates Virtual Haskell Environment in the current directory.\n"
                  ++ "All files will be stored in the .hsenv/ subdirectory."
