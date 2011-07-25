module Util.Tar ( unpack
                , stripComponents
                ) where

import Codec.Archive.Tar.Entry
import Codec.Archive.Tar (Entries(..), mapEntries)
-- import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Check (checkSecurity)
-- import Codec.Compression.BZip (decompress)

import qualified Data.ByteString.Lazy as BS
import System.FilePath ((</>), splitPath, joinPath, hasTrailingPathSeparator)
import qualified System.FilePath as FilePath.Native (takeDirectory)
import System.Directory (createDirectoryIfMissing, copyFile)
import Control.Monad (when)
import System.Posix.Files (ownerExecuteMode, intersectFileModes)

import Util.IO (makeExecutable)

isExecutable :: Permissions -> Bool
isExecutable perms = intersectFileModes ownerExecuteMode perms == ownerExecuteMode

unpack :: FilePath -> Entries -> IO ()
unpack baseDir entries = unpackEntries [] (checkSecurity entries)
                     >>= emulateLinks

  where
    unpackEntries _     (Fail err)      = fail err
    unpackEntries links Done            = return links
    unpackEntries links (Next entry es) = case entryContent entry of
      NormalFile file _ -> extractFile path file (entryPermissions entry)
                        >> unpackEntries links es
      Directory         -> extractDir path
                        >> unpackEntries links es
      HardLink     link -> (unpackEntries $! saveLink path link links) es
      SymbolicLink link -> (unpackEntries $! saveLink path link links) es
      _                 -> unpackEntries links es --ignore other file types
      where
        path = entryPath entry

    extractFile path content perms = do
      createDirectoryIfMissing True absDir
      BS.writeFile absPath content
      when (isExecutable perms) $ makeExecutable absPath
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractDir path = createDirectoryIfMissing True (baseDir </> path)

    saveLink path link links = seq (length path)
                             $ seq (length link')
                             $ (path, link'):links
      where link' = fromLinkTarget link

    emulateLinks = mapM_ $ \(relPath, relLinkTarget) ->
      let absPath   = baseDir </> relPath
          absTarget = FilePath.Native.takeDirectory absPath </> relLinkTarget
       in copyFile absTarget absPath

stripComponents :: Int -> Entries -> Entries
stripComponents n = mapEntries aux
  where aux entry@Entry{entryTarPath = oldTarPath} =
          let isDirectory = hasTrailingPathSeparator $ fromTarPath oldTarPath
          in case toTarPath isDirectory $ joinPath $ drop n $ splitPath $ fromTarPath oldTarPath of
            Left err         -> Left err
            Right newTarPath -> Right entry{entryTarPath = newTarPath}

-- foo :: (Entry -> a) -> Entries -> [a]
-- foo _ (Fail _) = []
-- foo _ Done = []
-- foo f (Next e es) = f e:foo f es

-- bar = do
--   tarBall <- BS.readFile "/tmp/ghc-7.0.4-i386-unknown-linux.tar.bz2"
--   let tar = decompress tarBall
--       tarContents = Tar.read tar
--   return $ foo entryPath tarContents
