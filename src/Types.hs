module Types ( GhcSource(..)
             , Options(..)
             , MyState(..)
             , DirStructure(..)
             ) where

data GhcSource = System           -- Use System's copy of GHC
               | Tarball FilePath -- Use GHC from tarball

data Options = Options { verbose   :: Bool
                       , vheName   :: String -- Virtual Haskell Environment name
                       , ghcSource :: GhcSource
                       }

data MyState = MyState { logDepth :: Integer -- used for indentation of logging messages
                       }

-- Only absolute paths!
data DirStructure = DirStructure { virthualEnv       :: FilePath -- dir containing .virthualenv dir (usually dir with cabal project)
                                 , virthualEnvDir    :: FilePath -- .virthualenv dir
                                 , ghcPackagePath    :: FilePath -- file (<ghc-6.12) or dir (>=ghc-6.12) containing private GHC pkg db
                                 , cabalDir          :: FilePath -- directory with private cabal dir
                                 , cabalBinDir       :: FilePath -- cabal's bin/ dir (used in $PATH)
                                 , virthualEnvBinDir :: FilePath -- dir with haskell tools wrappers and activate script
                                 , tmpDir            :: FilePath -- tmp dir, deleted on exit
                                 , ghcDir            :: FilePath -- directory with private copy of external GHC (only used when using GHC from tarball)
                                 , ghcBinDir         :: FilePath -- ghc's bin/ dir (with ghc[i|-pkg]) (only used when using GHC from tarball)
                                 }
