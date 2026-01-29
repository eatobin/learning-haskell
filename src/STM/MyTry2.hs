module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

main = do
  shared <- atomically (newTVar 70)
  -- before <- atomRead shared
  -- putStrLn $ "Before: " ++ show before
  -- appV (+ 22) shared
  -- dispVar shared
  -- appV (* 3) shared
  -- after <- atomRead shared
  -- after <- atomically (readTVar shared)
  -- putStrLn ("After: " ++ show after)after <- atomically (readTVar shared)
  -- putStrLn ("After: " ++ show after)
  -- atomically (writeTVar shared 59)
  appV (+ 22) shared
  -- atomically (readTVar shared >>= \j -> writeTVar shared (j + 29))
  atomically (readTVar shared) >>= print

-- atomRead = atomically . readTVar
-- dispVar x = atomRead x >>= print
-- appV fn x = atomically $ readTVar x >>= writeTVar x . fn
appV fn x = atomically (readTVar x >>= writeTVar x . fn)

-- appV x = atomically (readTVar x >>= writeTVar x . (+ 1))
-- appV x = atomically (readTVar x >>= \j -> writeTVar x (j + 20))
-- appV x = atomically (readTVar x >>= \j -> writeTVar x (j + 20))
