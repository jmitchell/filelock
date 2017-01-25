import Control.Concurrent
import Debug.Trace
import System.FileLock

main :: IO ()
main = lockAndWriteFile "testLockFile.txt" "testing, testing; 1, 2, 3"

lockAndWriteFile :: String -> String -> IO ()
lockAndWriteFile fname contents = trace "Test.lockAndWriteFile: start" $ do
  withFileLock fname Exclusive $ \_ -> do
    putStrLn "  attempting write..."

    -- Following line fails, printing "test-filelock.exe: testLockFile.txt:
    -- hClose: permission denied (Permission denied)" and process exits
    -- prematurely with exit code 1.
    --
    -- However, there are no issues when it's commented out.
    writeFile fname contents

  -- Never prints (unless writeFile line is commented out).
  putStrLn "released exclusive lock"
  trace "Test.lockAndWriteFile: end" $ return ()
