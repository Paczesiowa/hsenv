module PackageManagement ( Transplantable(..)
                         , parseVersion
                         , parsePkgInfo
                         , getHighestVersion
                         , GhcPkg
                         , outsideGhcPkg
                         , insideGhcPkg
                         ) where

import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Version (Version(..))
import Control.Monad (unless)

import Types
import MyMonad
import Process (outsideProcess, insideProcess)
import Util.Cabal (prettyPkgInfo, prettyVersion)
import qualified Util.Cabal (parseVersion, parsePkgInfo)

type GhcPkg = [String] -> Maybe String -> MyMonad String

outsideGhcPkg :: GhcPkg
outsideGhcPkg = outsideProcess "ghc-pkg"

insideGhcPkg :: GhcPkg
insideGhcPkg = insideProcess "ghc-pkg"

parseVersion :: String -> MyMonad Version
parseVersion s = case Util.Cabal.parseVersion s of
                    Nothing      -> throwError $ MyException $ "Couldn't parse " ++ s ++ " as a package version"
                    Just version -> return version

parsePkgInfo :: String -> MyMonad PackageIdentifier
parsePkgInfo s = case Util.Cabal.parsePkgInfo s of
                   Nothing      -> throwError $ MyException $ "Couldn't parse package identifier " ++ s
                   Just pkgInfo -> return pkgInfo

getDeps :: PackageIdentifier -> MyMonad [PackageIdentifier]
getDeps pkgInfo = do
  let prettyPkg = prettyPkgInfo pkgInfo
  debug $ "Extracting dependencies of " ++ prettyPkg
  out <- indentMessages $ outsideGhcPkg ["field", prettyPkg, "depends"] Nothing
  -- example output:
  -- depends: ghc-prim-0.2.0.0-3fbcc20c802efcd7c82089ec77d92990
  --          integer-gmp-0.2.0.0-fa82a0df93dc30b4a7c5654dd7c68cf4 builtin_rts
  case words out of
    []           -> throwError $ MyException $ "Couldn't parse ghc-pkg output to find dependencies of " ++ prettyPkg
    _:depStrings -> do -- skip 'depends:'
      indentMessages $ trace $ "Found dependency strings: " ++ unwords depStrings
      mapM parsePkgInfo depStrings

-- things that can be copied from system's GHC pkg database
-- to GHC pkg database inside virtual environment
class Transplantable a where
    transplantPackage :: a -> MyMonad ()

getHighestVersion :: PackageName -> GhcPkg -> MyMonad Version
getHighestVersion (PackageName packageName) ghcPkg = do
  debug $ "Checking the highest installed version of package " ++ packageName
  out <- indentMessages $ ghcPkg ["field", packageName, "version"] Nothing
  -- example output:
  -- version: 1.1.4
  -- version: 1.2.0.3
  let extractVersionString :: String -> MyMonad String
      extractVersionString line =
          case words line of
            [_, x] -> return x
            _   -> throwError $ MyException $ "Couldn't extract version string from: " ++ line
  versionStrings <- mapM extractVersionString $ lines out
  indentMessages $ trace $ "Found version strings: " ++ unwords versionStrings
  versions <- mapM parseVersion versionStrings
  case versions of
    []     -> throwError $ MyException $ "No versions of package " ++ packageName ++ " found"
    (v:vs) -> do
      indentMessages $ debug $ "Found: " ++ unwords (map prettyVersion versions)
      return $ foldr max v vs

-- choose the highest installed version of package with this name
instance Transplantable PackageName where
    transplantPackage pkg@(PackageName packageName) = do
      debug $ "Copying package " ++ packageName ++ " to Virtual Haskell Environment."
      indentMessages $ do
        highestVersion <- getHighestVersion pkg outsideGhcPkg
        debug $ "Using version: " ++ prettyVersion highestVersion
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
  out <- outsideGhcPkg ["describe", prettyPkg] Nothing
  _ <- insideGhcPkg ["register", "-"] (Just out)
  return ()
