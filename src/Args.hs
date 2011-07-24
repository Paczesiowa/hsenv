module Args ( usage
            , parseArgs
            ) where

import System.Directory (getCurrentDirectory)
import Data.List (isPrefixOf)
import Control.Monad (when)
import System.Environment (getProgName)
import System.FilePath (splitPath)

import Types

usage :: IO ()
usage = do
    name <- getProgName
    putStrLn $ "usage: " ++ name ++ " [FLAGS]"
    putStrLn ""
    putStrLn "Flags:"
    putStrLn "-h --help Show this help message"
    putStrLn "--verbose Print some debugging info"
    putStrLn "--name=NAME Use Name for name of Virthual Haskell Environment"
    putStrLn "            (defaults to the name of the current directory)"
    putStrLn "--ghc=X     X can be one of:"
    putStrLn "              system - Virtual Haskell Environment will use"
    putStrLn "                       system copy of ghc (and cabal)"
    putStrLn "                       this is the default behaviour"
    putStrLn "              FILE   - where FILE is a path to a tarball with"
    putStrLn "                       ghc (e.g. ghc-7.0.4-i386-unknown-linux.tar.bz2)"
    putStrLn ""
    putStrLn "Creates Virtual Haskell Environment in the current directory."
    putStrLn "All files will be stored in the .virthualenv/ subdirectory."

parseArgs :: [String] -> IO (Maybe Options)
parseArgs args = do
  let (verbosityFlags, nonVerbosityFlags) = span (`elem` ["--verbose", "--very-verbose"]) args
      verboseness = case verbosityFlags of
                      "--verbose":_      -> Verbose
                      "--very-verbose":_ -> VeryVerbose
                      _                  -> Quiet
      (nameFlags, nonNameFlags) = span ("--name=" `isPrefixOf`) nonVerbosityFlags
  name <- case nameFlags of
           nameFlag:_ -> return $ drop (length "--name=") nameFlag
           [] -> do
             cwd <- getCurrentDirectory
             let dirs = splitPath cwd
                 name = last dirs
             when (verboseness > Quiet) $ putStrLn $ "Using current directory name as Virtual Haskell Environment name: " ++ name
             return name
  let (ghcSourceFlags, restOfFlags) = span ("--ghc=" `isPrefixOf`) nonNameFlags
      ghc = case ghcSourceFlags of
              []     -> System
              path:_ -> Tarball $ drop (length "--ghc=") path
  case restOfFlags of
    [] -> return $ Just Options { verbosity = verboseness
                               , vheName   = name
                               , ghcSource = ghc
                               }
    _ -> return Nothing
