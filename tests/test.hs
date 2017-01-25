{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Debug.Trace (trace)
import System.Environment
import System.Exit
import System.Process

import System.FileLock

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["shared", read -> duration]
      -> holdLock "shared" Shared duration
    ["exclusive", read -> duration]
      -> holdLock "exclusive" Exclusive duration
    ["try"]
      -> tryTakingLock
    _ -> void $ mapConcurrently id
      [ callSelf ["shared", "300"]
      , callSelf ["shared", "200"]
      , msleep 10 >> callSelf ["exclusive", "500"]
      , msleep 20 >> callSelf ["try"]
      , msleep 50 >> callSelf ["shared", "500"]
      , msleep 700 >> callSelf ["shared", "10"]
      , msleep 1500 >> callSelf ["try"]
      ]

callSelf :: [String] -> IO ()
callSelf args = do
  result <- trace "Attempting to call self" $ rawSystem ".\\bin\\test-filelock.exe" args
  case result of
    ExitSuccess -> putStrLn "callSelf succeeded"
    x -> putStrLn $ "callSelf encountered unexpected exit code: " ++ show x
  return ()

msleep :: Int -> IO ()
msleep = threadDelay . (*1000)

holdLock :: String -> SharedExclusive -> Int -> IO ()
holdLock ty sex duration = do
  withFileLock lockfile sex $ \_ -> do
    putStrLn $ "took " ++ desc
    if sex == Exclusive then testWrite else return ()
    msleep duration
  putStrLn $ "released " ++ desc
  where
    desc = ty ++ " lock"
    testWrite = trace "attempting write..." $ writeFile lockfile "testing"

tryTakingLock :: IO ()
tryTakingLock = do
  ml <- tryLockFile lockfile Exclusive
  case ml of
    Nothing -> putStrLn "lock not available"
    Just l -> do
      putStrLn "lock was available"
      unlockFile l

lockfile :: String
lockfile = "lock"
