module System.FileLock.Internal.LockFileEx
#ifndef USE_LOCKFILEEX
  () where
#else
  (Lock, lock, -- tryLock,
   unlock) where

#include <windows.h>

import Control.Applicative
import qualified Control.Exception as E
import Data.Bits
import Debug.Trace
import Foreign.Marshal.Alloc
import System.Win32.File
import System.Win32.Mem
import System.Win32.Types

type Lock = HANDLE

lock :: FilePath -> Bool -> IO Lock
lock path exclusive = trace "LFE.lock: start" $ do
  file <- open path
  (`E.onException` closeHandle file) $ do
    True <- lockFirstByte file exclusive True
    trace "LFE.lock: end" $ return file

-- tryLock :: FilePath -> Bool -> IO (Maybe Lock)
-- tryLock path exclusive = do
--   file <- open path
--   (`E.onException` closeHandle file) $ do
--     r <- lockFirstByte file exclusive False
--     if r
--       then return $ Just file
--       else Nothing <$ closeHandle file

unlock :: Lock -> IO ()
unlock lock = do
  trace "LFE.unlock: start" $ closeHandle lock
  trace "LFE.unlock: end" $ return ()

open :: FilePath -> IO HANDLE
open path = trace "LFE.open: start" $ do
  h <- createFile path gENERIC_WRITE (fILE_SHARE_READ .|. fILE_SHARE_WRITE)
                  Nothing oPEN_ALWAYS fILE_ATTRIBUTE_NORMAL Nothing
  trace "LFE.open: end" $ return h

lockFirstByte :: HANDLE -> Bool -> Bool -> IO Bool
lockFirstByte handle exclusive block
    = trace "LFE.lockFirstByte: start" $ allocaBytes sizeof_OVERLAPPED $ \op -> do
  zeroMemory op $ fromIntegral sizeof_OVERLAPPED
  -- Offset and OffsetHigh fields are set to 0 by zeroMemory.
  r <- c_lockFileEx handle (exFlag .|. blockFlag) 0{-reserved-}
    1{-number of bytes, lower dword-}
    0{-number of bytes, higher dword-}
    op
  if r
    then trace "LFE.lockFirstByte: end (success)" $ return True -- success
    else do
      code <- getLastError
      if code == #{const ERROR_LOCK_VIOLATION}
        then trace "LFE.lockFirstByte: end (fail: already taken)" $ return False -- already taken
        else trace "LFE.lockFirstByte: end (fail: other)" $ failWith "LockFileEx" code
  where
    exFlag = if exclusive then #{const LOCKFILE_EXCLUSIVE_LOCK} else 0
    blockFlag = if block then 0 else #{const LOCKFILE_FAIL_IMMEDIATELY}
    sizeof_OVERLAPPED = #{size OVERLAPPED}

foreign import stdcall "LockFileEx" c_lockFileEx
  :: HANDLE -> DWORD -> DWORD -> DWORD -> DWORD -> LPOVERLAPPED -> IO BOOL

#endif /* USE_LOCKFILEEX */
