{-# LANGUAGE Arrows, CPP #-}

module Args (getArgs) where

import Control.Arrow
import Util.Args
import System.Directory (getCurrentDirectory)
import System.FilePath (splitPath)
import Types

#ifdef cabal
import Util.Cabal (prettyVersion)
import Paths_hsenv (version)

versionString :: String
versionString = prettyVersion version
#else
versionString :: String
versionString = "dev"
#endif

verbosityOpt, veryVerbosityOpt, skipSanityOpt, sharingOpt, bootstrapCabalOpt :: Switch

verbosityOpt = Switch { switchName  = "verbose"
                      , switchHelp  = "Print some debugging info"
                      , switchShort = Just 'v'
                      }

veryVerbosityOpt = Switch { switchName  = "very-verbose"
                          , switchHelp  = "Print some more debugging info"
                          , switchShort = Nothing
                          }

skipSanityOpt = Switch { switchName  = "skip-sanity-check"
                       , switchHelp  = "Skip all the sanity checks (use at your own risk)"
                       , switchShort = Nothing
                       }

sharingOpt = Switch { switchName  = "dont-share-cabal-cache"
                    , switchHelp  = "Don't share ~/.cabal/packages (hackage download cache)"
                    , switchShort = Nothing
                    }

bootstrapCabalOpt =
    Switch { switchName  = "bootstrap-cabal"
           , switchHelp  = "Bootstrap cabal-install inside virtual environment"
                           ++ "(Use this if you don't have cabal-install installed "
                           ++ "or it's not on your $PATH)"
           , switchShort = Nothing
           }

nameOpt, ghcOpt :: DynOpt

nameOpt = DynOpt
          { dynOptName = "name"
          , dynOptTemplate = "NAME"
          , dynOptDescription = "current directory name"
          , dynOptHelp = "Use NAME as name of the Virtual Haskell Environment"
          }

ghcOpt = DynOpt
         { dynOptName = "ghc"
         , dynOptTemplate = "FILE"
         , dynOptDescription = "system's copy of GHC"
         , dynOptHelp =
             "Use GHC from provided tarball (e.g. ghc-7.0.4-i386-unknown-linux.tar.bz2)"
         }

makeOpt :: StaticOpt
makeOpt = StaticOpt
          { staticOptName = "make-cmd"
          , staticOptTemplate = "CMD"
          , staticOptDefault = "make"
          , staticOptHelp =
              "Used as make substitute for installing GHC from tarball (e.g. gmake)"
          }

argParser :: ArgArrow () Options
argParser = proc () -> do
  verbosityFlag <- getOpt verbosityOpt -< ()
  verbosityFlag2 <- getOpt veryVerbosityOpt -< ()
  let verboseness = case (verbosityFlag, verbosityFlag2) of
                      (_, True)      -> VeryVerbose
                      (True, False)  -> Verbose
                      (False, False) -> Quiet
  nameFlag <- getOpt nameOpt -< ()
  name <- case nameFlag of
           Just name' -> returnA -< name'
           Nothing -> do
             cwd <- liftIO' getCurrentDirectory -< ()
             returnA -< last $ splitPath cwd
  ghcFlag <- getOpt ghcOpt -< ()
  let ghc = case ghcFlag of
              Nothing   -> System
              Just path -> Tarball path
  skipSanityCheckFlag <- getOpt skipSanityOpt -< ()
  noSharingFlag <- getOpt sharingOpt -< ()
  bootstrapCabalFlag <- getOpt bootstrapCabalOpt -< ()
  make <- getOpt makeOpt -< ()
  returnA -< Options{ verbosity       = verboseness
                   , skipSanityCheck = skipSanityCheckFlag
                   , hsEnvName       = name
                   , ghcSource       = ghc
                   , makeCmd         = make
                   , noSharing       = noSharingFlag
                   , cabalBootstrap  = bootstrapCabalFlag
                   }
    where liftIO' = liftIO . const

getArgs :: IO Options
getArgs = parseArgs argParser versionString outro
    where outro = "Creates Virtual Haskell Environment in the current directory.\n"
                  ++ "All files will be stored in the .hsenv_ENVNAME/ subdirectory."
