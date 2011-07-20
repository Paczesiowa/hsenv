{-# LANGUAGE TemplateHaskell #-}

module Skeletons where

import Data.FileEmbed (embedFile)
import Data.ByteString.Char8 (unpack)

activateSkel :: String
activateSkel = unpack $(embedFile "activate")

cabalWrapperSkel :: String
cabalWrapperSkel = unpack $(embedFile "cabal")

cabalConfigSkel :: String
cabalConfigSkel = unpack $(embedFile "cabal_config")
