module PackageManagement (Transplantable(..)) where

import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Data.Maybe (mapMaybe)
import Control.Monad (unless)
import Control.Monad.Error (catchError)

import MyMonad
import Process (outsideGhcPkg, insideGhcPkg)
import Util.Cabal (prettyPkgInfo, prettyVersion, parseVersion, parsePkgInfo)

getDeps :: PackageIdentifier -> MyMonad [PackageIdentifier]
getDeps pkgInfo = do
  debug $ "Extracting dependencies of " ++ prettyPkgInfo pkgInfo
  x <- outsideGhcPkg ["field", prettyPkgInfo pkgInfo, "depends"]
  let depStrings = tail $ words x
      deps = mapMaybe parsePkgInfo depStrings
  return deps

-- things that can be copied from system's GHC pkg database
-- to GHC pkg database inside virtual environment
class Transplantable a where
    transplantPackage :: a -> MyMonad ()

-- choose the highest installed version of package with this name
instance Transplantable PackageName where
    transplantPackage (PackageName packageName) = do
      debug $ "Copying package " ++ packageName ++ " to Virtual Haskell Environment."
      indentMessages $ do
        debug "Choosing package with highest version number."
        out <- indentMessages $ outsideGhcPkg ["field", packageName, "version"]
        -- example output:
        -- version: 1.1.4
        -- version: 1.2.0.3
        let versionStrings = map (\line -> words line !! 1) $ lines out
            versions       = mapMaybe parseVersion versionStrings
        indentMessages $ debug $ "Found: " ++ unwords (map prettyVersion versions)
        let version = maximum versions
        indentMessages $ debug $ "Using version: " ++ prettyVersion version
        let pkgInfo = PackageIdentifier (PackageName packageName) version
        transplantPackage pkgInfo

-- check if this package is already installed in Virtual Haskell Environment
checkIfInstalled :: PackageIdentifier -> MyMonad Bool
checkIfInstalled pkgInfo = do
  let package = prettyPkgInfo pkgInfo
  debug $ "Checking if " ++ package ++ " is already installed."
  (do
    _ <- indentMessages $ insideGhcPkg ["describe", package] Nothing
    indentMessages $ debug "It is."
    return True) `catchError` handler
   where handler _ = do
           debug "It's not."
           return False

instance Transplantable PackageIdentifier where
    transplantPackage pkgInfo = do
      debug $ "Copying package " ++ prettyPkgInfo pkgInfo ++ " to Virtual Haskell Environment."
      indentMessages $ do
        flag <- checkIfInstalled pkgInfo
        unless flag $ do
          deps <- getDeps pkgInfo
          debug $ "Found: " ++ unwords (map prettyPkgInfo deps)
          mapM_ transplantPackage deps
          movePackage pkgInfo

-- copy single package that already has all deps satisfied
movePackage :: PackageIdentifier -> MyMonad ()
movePackage pkgInfo = do
  let package = prettyPkgInfo pkgInfo
  debug $ "Moving package " ++ prettyPkgInfo pkgInfo ++ " to Virtual Haskell Environment."
  out <- outsideGhcPkg ["describe", package]
  _ <- insideGhcPkg ["register", "-"] (Just out)
  return ()
