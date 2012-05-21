module Util.Args.RawArgs where

import Data.Monoid
import Util.List (breakOn)

{-# ANN Args "HLint: ignore Use String" #-}
data Args = Args { shortSwitches :: [Char]
                 , switches      :: [String]
                 , valArgs       :: [(String, String)]
                 }
    deriving Show

instance Monoid Args where
    mempty = Args [] [] []
    Args xs1 ys1 zs1 `mappend` Args xs2 ys2 zs2 = Args (xs1 ++ xs2) (ys1 ++ ys2) (zs1 ++ zs2)

parseArgument :: String -> Args
parseArgument ('-':'-':arg) =
  case breakOn '=' arg of
    Nothing         -> mempty{switches = [arg]}
    Just (key, val) -> mempty{valArgs = [(key, val)]}
parseArgument ['-', c] = mempty{shortSwitches = [c]}
parseArgument ('-':_k:_val) = mempty
parseArgument _ = mempty

parseArguments :: [String] -> Args
parseArguments args =
    case breakOn "--" args of
      Nothing -> mconcat $ map parseArgument args
      Just (args', _positionals) -> mconcat $ map parseArgument args'
