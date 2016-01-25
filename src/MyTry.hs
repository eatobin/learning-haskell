module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

main = do shared <- atomically $ newTVar 0
          before <- atomRead shared
          putStrLn $ "Before: " ++ show before
          appV (+ 22) shared
          dispVar shared
          appV (* 3) shared
          after <- atomRead shared
          putStrLn $ "After: " ++ show after

atomRead = atomically . readTVar
dispVar x = atomRead x >>= print
appV fn x = atomically $ readTVar x >>= writeTVar x . fn
