module CabalBootstrap (bootstrapCabal) where

import System.Directory (doesFileExist, createDirectoryIfMissing, getCurrentDirectory, getAppUserDataDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import Network.URI (URI(..), URIAuth(..))
import Network.HTTP
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS
import Distribution.Hackage.DB hiding (map, foldr)
import Prelude hiding (filter)
import qualified Codec.Archive.Tar as Tar

import Types
import MyMonad
import MyMonadUtils
import Paths
import Process
import PackageManagement
import Util.Cabal (prettyVersion, executableMatchesCabal)

hackageDomain :: String
hackageDomain = "hackage.haskell.org"

indexURI :: URI
indexURI = URI { uriScheme    = "http:"
               , uriAuthority = Just URIAuth { uriUserInfo = ""
                                             , uriRegName  = hackageDomain
                                             , uriPort     = ""
                                             }
               , uriPath      = "/packages/archive/00-index.tar.gz"
               , uriQuery     = ""
               , uriFragment  = ""
               }

getCIBURI :: Version -> URI
getCIBURI version = indexURI {uriPath = path}
    where path = concat [ "/packages/archive/cabal-install-bundle/"
                        , ver
                        , "/cabal-install-bundle-"
                        , ver
                        , ".tar.gz"
                        ]
          ver = prettyVersion version

downloadHTTPUncompress :: URI -> MyMonad BS.ByteString
downloadHTTPUncompress uri = do
  result <- liftIO $ simpleHTTP $ mkRequest GET uri
  case result of
    Left err -> throwError $ MyException $ show err
    Right response -> return $ decompress $ rspBody response

fetchHackageIndex :: MyMonad ()
fetchHackageIndex = do
  debug "Checking if Hackage index is already downloaded"
  noSharingFlag <- asks noSharing
  dirStructure  <- hseDirStructure
  hackageCache  <- indentMessages $
      if noSharingFlag then
          return $ cabalDir dirStructure </> "packages"
      else do
          cabalInstallDir <- liftIO $ getAppUserDataDirectory "cabal"
          return $ cabalInstallDir </> "packages"
  let cacheDir = hackageCache </> hackageDomain
      hackageData = cacheDir </> "00-index.tar"
  liftIO $ putStrLn hackageData
  dataExists <- liftIO $ doesFileExist hackageData
  if dataExists then
    indentMessages $ debug "It is"
   else do
    indentMessages $ debug "It's not"
    info "Downloading Hackage index"
    liftIO $ createDirectoryIfMissing True cacheDir
    tarredIndex <- downloadHTTPUncompress indexURI
    liftIO $ BS.writeFile hackageData tarredIndex

readHackageIndex :: MyMonad Hackage
readHackageIndex = do
  noSharingFlag <- asks noSharing
  dirStructure  <- hseDirStructure
  hackageCache  <- indentMessages $
      if noSharingFlag then
          return $ cabalDir dirStructure </> "packages"
      else do
          cabalInstallDir <- liftIO $ getAppUserDataDirectory "cabal"
          return $ cabalInstallDir </> "packages"
  let cacheDir = hackageCache </> hackageDomain
      hackageIndexLocation = cacheDir </> "00-index.tar"
  liftIO $ readHackage' hackageIndexLocation

chooseCIBVersion :: Hackage -> MyMonad Version
chooseCIBVersion hackage = do
  debug "Choosing the right cabal-install-bundle version"
  cabalVersion <- getHighestVersion (PackageName "Cabal") insideGhcPkg
  debug $ "Cabal library has version " ++ prettyVersion cabalVersion
  let cibs = hackage ! "cabal-install-bundle"
      cibVersions = keys cibs
  trace $ "Found cabal-install-bundle versions: " ++ unwords (map prettyVersion cibVersions)
  let matchingCIBs = filter (executableMatchesCabal "cabal" cabalVersion) cibs
      matchingCIBVersions = keys matchingCIBs
  debug $ "cabal-install-bundle versions matching Cabal library: "
            ++ unwords (map prettyVersion matchingCIBVersions)
  case matchingCIBVersions of
    [] -> throwError $ MyException $ "No cabal-install-bundle packages "
                           ++ "matching installed Cabal library"
    v:vs -> return $ foldr max v vs

bootstrapCabal :: MyMonad ()
bootstrapCabal = do
  fetchHackageIndex
  hackageIndex <- readHackageIndex
  cibVersion <- chooseCIBVersion hackageIndex
  info $ "Using cabal-install-bundle version " ++ prettyVersion cibVersion
  let url = getCIBURI cibVersion
  trace $ "Download URL: " ++ show url
  tarredPkg <- downloadHTTPUncompress url
  dirStructure <- hseDirStructure
  let prefix = cabalDir dirStructure
  runInTmpDir $ do
    cwd <- liftIO getCurrentDirectory
    trace $ "Unpacking package in " ++ cwd
    liftIO $ Tar.unpack cwd $ Tar.read tarredPkg
    debug "Configuring cabal-install-bundle"
    let pkgDir = cwd </> "cabal-install-bundle-" ++ prettyVersion cibVersion
        setup  = pkgDir </> "Setup.hs"
    liftIO $ setCurrentDirectory pkgDir
    let cabalSetup args = insideProcess "runghc" (setup:args) Nothing
    _ <- cabalSetup ["configure", "--prefix=" ++ prefix, "--user"]
    debug "Building cabal-install-bundle"
    _ <- cabalSetup ["build"]
    debug "Installing cabal-install-bundle"
    _ <- cabalSetup ["install"]
    return ()
