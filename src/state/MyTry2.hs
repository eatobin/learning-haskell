module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

main = do shared <- atomically (newTVar 555667)
          --before <- atomRead shared
          --putStrLn $ "Before: " ++ show before
          --appV (+ 22) shared
          --dispVar shared
          --appV (* 3) shared
          --after <- atomRead shared
          --after <- atomically (readTVar shared)
          --putStrLn ("After: " ++ show after)after <- atomically (readTVar shared)
          --putStrLn ("After: " ++ show after)
          atomically (readTVar shared) >>= writeTVar ((+ 5) shared)
          atomically (readTVar shared) >>= print

--atomRead = atomically . readTVar
--dispVar x = atomRead x >>= print
--appV fn x = atomically $ readTVar x >>= writeTVar x . fn
--appV fn = atomically (readTVar shared) >>= (writeTVar shared) (+5)
