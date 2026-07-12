module NewMo where

import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    execState,
    runState,
  )

type Score = Int

-- No manual state arguments or tuple returns in the type
scorePointState :: State Score String
scorePointState = do
  currentScore <- get
  let newScore = currentScore + 10
  put newScore
  return "Point scored!"

bonusPointState :: State Score String
bonusPointState = do
  currentScore <- get
  let newScore = currentScore + 50
  put newScore
  return "Bonus awarded!"

-- Automatic state threading using do-notation
runGameState :: State Score String
runGameState = do
  msg1 <- scorePointState
  msg2 <- bonusPointState
  return (msg1 ++ " " ++ msg2)

main :: IO ()
main = do
  let initialState = 0

  -- 1. Get both the message and the final score
  print $ runState runGameState initialState
  -- Output: ("Point scored! Bonus awarded!", 60)

  -- 2. Get only the message
  print $ evalState runGameState initialState
  -- Output: "Point scored! Bonus awarded!"

  -- 3. Get only the final score
  print $ execState runGameState initialState

-- Output: 60
