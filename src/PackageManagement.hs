module PackageManagement (Transplantable(..)) where

import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Version (Version(..))
import Control.Monad.Error (throwError, catchError)
import Control.Monad (unless)

import Types
import MyMonad
import Process (outsideGhcPkg, insideGhcPkg)
import Util.Cabal (prettyPkgInfo, prettyVersion, parseVersion, parsePkgInfo)

getDeps :: PackageIdentifier -> MyMonad [PackageIdentifier]
getDeps pkgInfo = do
  let prettyPkg = prettyPkgInfo pkgInfo
  debug $ "Extracting dependencies of " ++ prettyPkg
  out <- indentMessages $ outsideGhcPkg ["field", prettyPkg, "depends"]
  -- example output:
  -- depends: ghc-prim-0.2.0.0-3fbcc20c802efcd7c82089ec77d92990
  --          integer-gmp-0.2.0.0-fa82a0df93dc30b4a7c5654dd7c68cf4 builtin_rts
  case words out of
    []           -> throwError $ MyException $ "Couldn't parse ghc-pkg output to find dependencies of " ++ prettyPkg
    _:depStrings -> do -- skip 'depends:'
      indentMessages $ trace $ "Found dependency strings: " ++ unwords depStrings
      let parsePkgInfo' :: String -> MyMonad PackageIdentifier
          parsePkgInfo' x = maybe (throwError $ MyException $ "Couldn't parse package identifier " ++ x)
                                  return
                                  (parsePkgInfo x)
      mapM parsePkgInfo' depStrings

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
        let extractVersionString :: String -> MyMonad String
            extractVersionString line = case words line of
                                          [_, x] -> return x
                                          _   -> throwError $ MyException $ "Couldn't extract version string from: " ++ line
        versionStrings <- mapM extractVersionString $ lines out
        indentMessages $ trace $ "Found version strings: " ++ unwords versionStrings
        let parseVersion' :: String -> MyMonad Version
            parseVersion' s = maybe (throwError $ MyException $ "Couldn't parse " ++ s ++ " as a package version")
                                    return
                                    (parseVersion s)
        versions <- mapM parseVersion' versionStrings
        case versions of
          []     -> throwError $ MyException $ "No versions of package " ++ packageName ++ " found"
          (v:vs) -> do
            indentMessages $ debug $ "Found: " ++ unwords (map prettyVersion versions)
            let highestVersion = foldr max v vs
            indentMessages $ debug $ "Using version: " ++ prettyVersion highestVersion
            let pkgInfo = PackageIdentifier (PackageName packageName) highestVersion
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
      let prettyPkg = prettyPkgInfo pkgInfo
      debug $ "Copying package " ++ prettyPkg ++ " to Virtual Haskell Environment."
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
  let prettyPkg = prettyPkgInfo pkgInfo
  debug $ "Moving package " ++ prettyPkg ++ " to Virtual Haskell Environment."
  out <- outsideGhcPkg ["describe", prettyPkg]
  _ <- insideGhcPkg ["register", "-"] (Just out)
  return ()
