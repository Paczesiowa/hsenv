{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types ( GhcSource(..)
             , Options(..)
             , MyState(..)
             , DirStructure(..)
             , MyException(..)
             , Verbosity(..)
             ) where

import Control.Monad.Error (Error)

data GhcSource = System           -- Use System's copy of GHC
               | Tarball FilePath -- Use GHC from tarball

data Verbosity = Quiet
               | Verbose
               | VeryVerbose
    deriving (Eq, Ord)

data Options = Options { verbosity       :: Verbosity
                       , skipSanityCheck :: Bool
                       , hsEnvName       :: String -- Virtual Haskell Environment name
                       , ghcSource       :: GhcSource
                       , makeCmd         :: String -- make substitute used for 'make install' of external GHC
                       }

data MyState = MyState { logDepth :: Integer -- used for indentation of logging messages
                       }

newtype MyException = MyException { getExceptionMessage :: String }
    deriving Error

-- Only absolute paths!
data DirStructure = DirStructure { hsEnv          :: FilePath -- dir containing .hsenv dir (usually dir with cabal project)
                                 , hsEnvDir       :: FilePath -- .hsenv dir
                                 , ghcPackagePath :: FilePath -- file (<ghc-6.12) or dir (>=ghc-6.12) containing private GHC pkg db
                                 , cabalDir       :: FilePath -- directory with private cabal dir
                                 , cabalBinDir    :: FilePath -- cabal's bin/ dir (used in $PATH)
                                 , hsEnvBinDir    :: FilePath -- dir with haskell tools wrappers and activate script
                                 , ghcDir         :: FilePath -- directory with private copy of external GHC (only used when using GHC from tarball)
                                 , ghcBinDir      :: FilePath -- ghc's bin/ dir (with ghc[i|-pkg]) (only used when using GHC from tarball)
                                 }
