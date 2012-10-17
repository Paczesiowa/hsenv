{-# LANGUAGE TemplateHaskell #-}

module Skeletons where

import Data.FileEmbed (embedFile)
import Data.ByteString.Char8 (unpack)
import System.FilePath ((</>))

activateSkel :: String
activateSkel = unpack $(embedFile $ "skeletons" </> "activate")

cabalWrapperSkel :: String
cabalWrapperSkel = unpack $(embedFile $ "skeletons" </> "cabal")

ghcpkgWrapperSkel:: String
ghcpkgWrapperSkel = unpack $(embedFile $ "skeletons" </> "ghc-pkg")

ghciWrapperSkel:: String
ghciWrapperSkel = unpack $(embedFile $ "skeletons" </> "ghci")

ghcWrapperSkel:: String
ghcWrapperSkel = unpack $(embedFile $ "skeletons" </> "ghc")

cabalConfigSkel :: String
cabalConfigSkel = unpack $(embedFile $ "skeletons" </> "cabal_config")
