{-# LANGUAGE TemplateHaskell #-}

module Skeletons where

import Data.FileEmbed (embedFile)
import Data.ByteString.Char8 (unpack)
import System.FilePath ((</>))

activateSkel :: String
activateSkel = unpack $(embedFile $ "skeletons" </> "activate")

cabalWrapperSkel :: String
cabalWrapperSkel = unpack $(embedFile $ "skeletons" </> "cabal")

cabalConfigSkel :: String
cabalConfigSkel = unpack $(embedFile $ "skeletons" </> "cabal_config")

ghcWrapperSkel :: String
ghcWrapperSkel = unpack $(embedFile $ "skeletons" </> "ghc")

ghciWrapperSkel :: String
ghciWrapperSkel = unpack $(embedFile $ "skeletons" </> "ghci")
