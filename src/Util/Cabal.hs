module Util.Cabal ( prettyVersion
                  , prettyPkgInfo
                  , parseVersion
                  , parsePkgInfo
                  ) where

import Distribution.Version (Version(..))
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Compat.ReadP (readP_to_S)
import Distribution.Text (parse, Text)

import Data.Char (isSpace)
import Data.List (isPrefixOf, intercalate)

-- render Version to human and ghc-pkg readable string
prettyVersion :: Version -> String
prettyVersion (Version [] _) = ""
prettyVersion (Version numbers _) = intercalate "." $ map show numbers

-- render PackageIdentifier to human and ghc-pkg readable string
prettyPkgInfo :: PackageIdentifier -> String
prettyPkgInfo (PackageIdentifier (PackageName name) (Version [] _)) = name
prettyPkgInfo (PackageIdentifier (PackageName name) version) =
  name ++ "-" ++ prettyVersion version

parseVersion :: String -> Maybe Version
parseVersion = parseCheck

parseCheck :: Text a => String -> Maybe a
parseCheck str =
  case [ x | (x,ys) <- readP_to_S parse str, all isSpace ys ] of
    [x] -> Just x
    _   -> Nothing

parsePkgInfo :: String -> Maybe PackageIdentifier
parsePkgInfo str | "builtin_" `isPrefixOf` str =
                     let name = drop (length "builtin_") str -- ghc-pkg doesn't like builtin_ prefix
                     in Just $ PackageIdentifier (PackageName name) $ Version [] []
                 | otherwise = parseCheck str
