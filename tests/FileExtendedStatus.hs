{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FileExtendedStatus (main) where

import System.Posix.Files
import System.Posix.Directory
import System.Posix.IO
import System.Posix.Types
import Control.Exception as E
import Control.Monad
import Test.Tasty.HUnit

main = do
  cleanup
  fs <- testRegular
  ds <- testDir
  testSymlink fs ds
  testLink
  cleanup

regular       = "regular"
dir           = "dir"
slink_regular = "link-regular-symlink"
hlink_regular = "link-regular-hardlink"
link_dir      = "link-dir"

testRegular = do
  _ <- createFile regular ownerReadMode
  (fs, _) <- getStatus regular
  let expected = (False,False,False,True,False,False,False)
      actual   = snd (statusExtendedElements fs)
  when (actual /= expected) $
    fail "unexpected file status bits for regular file"
  return fs

testDir = do
  createDirectory dir ownerReadMode
  (ds, _) <- getStatus dir
  let expected = (False,False,False,False,True,False,False)
      actual   = snd (statusExtendedElements ds)
  when (actual /= expected) $
    fail "unexpected file status bits for directory"
  return ds

testSymlink fs ds = do
  createSymbolicLink regular slink_regular
  createSymbolicLink dir     link_dir
  (fs', ls)  <- getStatus slink_regular
  (ds', lds) <- getStatus link_dir

  let expected = (False,False,False,False,False,True,False)
      actualF  = snd (statusExtendedElements ls)
      actualD  = snd (statusExtendedElements lds)

  when (actualF /= expected) $
    fail "unexpected file status bits for symlink to regular file"

  when (actualD /= expected) $
    fail "unexpected file status bits for symlink to directory"

  when (statusExtendedElements fs /= statusExtendedElements fs') $
    fail "status for a file does not match when it's accessed via a symlink"

  when (statusExtendedElements ds /= statusExtendedElements ds') $
    fail "status for a directory does not match when it's accessed via a symlink"


testLink = do
  createLink regular hlink_regular
  (fs, _)  <- getStatus regular -- we need to retrieve it again as creating the link causes it to change!
  (fs', ls)  <- getStatus hlink_regular
  snd (statusExtendedElements ls) @?= (
                False, -- isBlockDevice
                False, -- isCharacterDevice
                False, -- isNamedPipe
                True,  -- isRegularFile
                False, -- isDirectory
                False, -- isSymbolicLink
                False) -- isSocket
  linkCountX fs' @?= 2
  statusExtendedElements fs @?= statusExtendedElements fs' -- status for a file should match when accessed via a link


cleanup = do
  ignoreIOExceptions $ removeDirectory dir
  mapM_ (ignoreIOExceptions . removeLink)
        [regular, hlink_regular, slink_regular, link_dir]

ignoreIOExceptions io = io `E.catch`
                        ((\_ -> return ()) :: IOException -> IO ())

getStatus f = do
  fs  <- getExtendedFileStatus Nothing f defaultStatxFlags defaultStatxMask
  ls  <- getExtendedFileStatus Nothing f SymlinkNoFollow defaultStatxMask
  fs' <- getFileStatus f

  statusExtendedElementsMinimal fs @?= statusElementsMinimal fs'

  return (fs, ls)

-- Yay for 20-element tuples!
statusExtendedElements fs = (,)
  (fileBlockSizeX fs
  ,linkCountX fs
  ,fileOwnerX fs
  ,fileGroupX fs
  ,fileModeX fs
  ,fileIDX fs
  ,fileSizeX fs
  ,accessTimeHiResX fs
  ,creationTimeHiResX fs
  ,statusChangeTimeHiResX fs
  ,modificationTimeHiResX fs
  )
  (isBlockDeviceX fs
  ,isCharacterDeviceX fs
  ,isNamedPipeX fs
  ,isRegularFileX fs
  ,isDirectoryX fs
  ,isSymbolicLinkX fs
  ,isSocketX fs
  )

statusExtendedElementsMinimal fs = (,)
  (fileModeX fs
  ,deviceIDX fs
  ,specialDeviceIDX fs
  ,linkCountX fs
  ,fileOwnerX fs
  ,fileGroupX fs
  ,COff (fromIntegral (fileSizeX fs))
  ,Just $ CBlkCnt (fromIntegral (fileBlocksX fs))
  ,accessTimeHiResX fs
  ,statusChangeTimeHiResX fs
  ,modificationTimeHiResX fs
  )
  (isBlockDeviceX fs
  ,isCharacterDeviceX fs
  ,isNamedPipeX fs
  ,isRegularFileX fs
  ,isDirectoryX fs
  ,isSymbolicLinkX fs
  ,isSocketX fs
  )

statusElementsMinimal fs = (,)
  (fileMode fs
  ,deviceID fs
  ,specialDeviceID fs
  ,linkCount fs
  ,fileOwner fs
  ,fileGroup fs
  ,fileSize fs
  ,fileBlocks fs
  ,accessTimeHiRes fs
  ,statusChangeTimeHiRes fs
  ,modificationTimeHiRes fs
  )
  (isBlockDevice fs
  ,isCharacterDevice fs
  ,isNamedPipe fs
  ,isRegularFile fs
  ,isDirectory fs
  ,isSymbolicLink fs
  ,isSocket fs
  )
