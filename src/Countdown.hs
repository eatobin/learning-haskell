module Countdown where

import Control.Monad (void)
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get, put),
    StateT,
    execStateT,
  )

-- Define the countdown function
countdown :: StateT Int IO ()
countdown = do
  n <- get -- Fetch the current state
  if n <= 0
    then put 0 -- Stop at 0
    else do
      liftIO $ print n -- Print current state
      put (n - 1) -- Update the state
      countdown -- Recurse

-- Execute the stateful computation with an initial state of 5
mainX :: IO ()
mainX = Control.Monad.void (execStateT countdown 5)
