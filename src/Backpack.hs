module Backpack where

import Control.Monad.State.Strict
  ( MonadState (get, put),
    State,
    runState,
  )

-- 1. Define the shape of our State
data InventoryState = InventoryState
  { items :: [String],
    totalWeight :: Double
  }
  deriving (Show)

-- 2. Define a function to modify state inside the Monad
-- It returns a String confirmation message, and mutates InventoryState
addItem :: String -> Double -> State InventoryState String
addItem itemName itemWeight = do
  -- 'get' fetches the current state snapshot
  currentState <- get

  let currentWeight = totalWeight currentState
  let maxWeight = 10.0

  if currentWeight + itemWeight > maxWeight
    then return ("Too heavy! Could not add " ++ itemName)
    else do
      -- 'put' overwrites the old state with a new state
      put $
        InventoryState
          { items = itemName : items currentState,
            totalWeight = currentWeight + itemWeight
          }
      return ("Successfully added " ++ itemName)

-- 3. Sequence multiple stateful operations inside a do-block
gameSession :: State InventoryState [String]
gameSession = do
  msg1 <- addItem "Iron Sword" 4.5
  msg2 <- addItem "Healing Potion" 1.0
  msg3 <- addItem "Heavy Gold Chest" 8.0 -- This one should fail due to weight limits
  return [msg1, msg2, msg3]

-- 4. Execute the monad
mainX :: IO ()
mainX = do
  let initialState = InventoryState {items = [], totalWeight = 0.0}

  -- runState returns both the final function output AND the final state tuple: (result, state)
  let (logMessages, finalState) = runState gameSession initialState

  putStrLn "--- Action Logs ---"
  print logMessages

  putStrLn "\n--- Final Player State ---"
  print finalState

-- λ> mainX
-- --- Action Logs ---
-- ["Successfully added Iron Sword","Successfully added Healing Potion","Too heavy! Could not add Heavy Gold Chest"]

-- --- Final Player State ---
-- InventoryState {items = ["Healing Potion","Iron Sword"], totalWeight = 5.5}
