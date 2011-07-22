module PackageManagement ( transplantPackage
                         , transplantPkg
                         ) where

import Distribution.Package (PackageIdentifier(..), PackageName(..))
import System.Exit (ExitCode(..))
import Data.Maybe (catMaybes)

import MyMonad
import Process (outsideGhcPkg, insideGhcPkg)
import Util.Cabal (prettyPkgInfo, prettyVersion, parseVersion, parsePkgInfo)

getDeps :: PackageIdentifier -> MyMonad [PackageIdentifier]
getDeps pkgInfo = do
  debug $ "Extracting dependencies of " ++ prettyPkgInfo pkgInfo
  (_, x, _) <- outsideGhcPkg ["field", prettyPkgInfo pkgInfo, "depends"]
  let depStrings = tail $ words x
      deps = catMaybes $ map parsePkgInfo depStrings
  return deps

-- transplant a package from simple name (e.g. base)
-- tries to guess the version
transplantPackage :: String -> MyMonad ()
transplantPackage package = do
  debug $ "Copying package " ++ package ++ " to Virtual Haskell Environment."
  indentMessages $ do
    debug $ "Choosing package with highest version number."
    (_, out, _) <- indentMessages $ outsideGhcPkg ["field", package, "version"]
    -- example output:
    -- version: 1.1.4
    -- version: 1.2.0.3
    let versionStrings = map (!!1) $ map words $ lines out
        versions = catMaybes $ map parseVersion versionStrings
    indentMessages $ debug $ "Found: " ++ unwords (map prettyVersion versions)
    let version = maximum versions
    indentMessages $ debug $ "Using version: " ++ prettyVersion version
    let pkgInfo = PackageIdentifier (PackageName package) version
    transplantPkg pkgInfo

-- check if this package is already installed in Virtual Haskell Environment
checkIfInstalled :: PackageIdentifier -> MyMonad Bool
checkIfInstalled pkgInfo = do
  let package = prettyPkgInfo pkgInfo
  debug $ "Checking if " ++ package ++ " is already installed."
  (exitCode, _, _) <- indentMessages $ insideGhcPkg ["describe", package] Nothing
  indentMessages $ case exitCode of
                 ExitSuccess -> do
                   debug "It is."
                   return True
                 ExitFailure _ -> do
                   debug "It's not."
                   return False

transplantPkg :: PackageIdentifier -> MyMonad ()
transplantPkg pkgInfo = do
  debug $ "Copying package " ++ prettyPkgInfo pkgInfo ++ " to Virtual Haskell Environment."
  indentMessages $ do
    flag <- checkIfInstalled pkgInfo
    if flag then
        return ()
     else do
      deps <- getDeps pkgInfo
      debug $ "Found: " ++ unwords (map prettyPkgInfo deps)
      mapM_ transplantPkg deps
      movePackage pkgInfo

-- copy single package that already has all deps satisfied
movePackage :: PackageIdentifier -> MyMonad ()
movePackage pkgInfo = do
  let package = prettyPkgInfo pkgInfo
  debug $ "Moving package " ++ prettyPkgInfo pkgInfo ++ " to Virtual Haskell Environment."
  (_, out, _) <- outsideGhcPkg ["describe", package]
  _ <- insideGhcPkg ["register", "-"] (Just out)
  return ()
