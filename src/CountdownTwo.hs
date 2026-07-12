module CountdownTwo where

import Control.Monad.State (MonadState (get, put), State, runState)

-- The state is an Int (the current countdown number).
-- The result of the computation is a list of Strings (the emitted outputs).
type CountdownState = State Int [String]

countdownStep :: CountdownState
countdownStep = do
  n <- get -- Get the current state
  if n <= 0
    then do
      return ["Blast off!"]
    else do
      put (n - 1) -- Update state (decrement)
      let msg = "Counting down: " ++ show n
      rest <- countdownStep -- Recursively call next step
      return (msg : rest) -- Cons the current output to the rest

main :: IO ()
main = do
  let (outputs, finalState) = runState countdownStep 5
  print outputs
  -- mapM_ putStrLn outputs
  putStrLn $ "Final state: " ++ show finalState
