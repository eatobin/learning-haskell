module Fruit where

import Control.Monad.State.Strict
  ( MonadState (get, put),
    State,
    modify,
    runState,
  )
import qualified Data.Map as M

-- 1. Define the State Types
type Item = String

type Quantity = Int

type Inventory = M.Map Item Quantity

-- An InventoryState is a State monad carrying 'Inventory' as state,
-- and returning a value of type 'a'.
type InventoryState a = State Inventory a

-- 2. Core Stateful Operations

-- Add stock to the inventory
addStock :: Item -> Quantity -> InventoryState ()
addStock item qty = modify (M.insertWith (+) item qty)

-- Sell an item from the inventory
-- Returns True if successful, False if out of stock
sellItem :: Item -> Quantity -> InventoryState Bool
sellItem item qty = do
  currentInv <- get
  let currentQty = M.findWithDefault 0 item currentInv
  if currentQty >= qty
    then do
      -- Deduct stock and update state
      put (M.insert item (currentQty - qty) currentInv)
      return True
    else
      return False

-- 3. Combine Actions Into a Business Flow
processOrders :: InventoryState (Bool, Bool)
processOrders = do
  addStock "Apples" 10
  addStock "Oranges" 5
  success1 <- sellItem "Apples" 3 -- Should succeed (7 left)
  success2 <- sellItem "Oranges" 6 -- Should fail (not enough stock)
  return (success1, success2)

-- 4. Execute the Program
mainX :: IO ()
mainX = do
  let initialInventory = M.empty

  -- runState returns a tuple containing: (Final Result, Final State)
  let (results, finalInventory) = runState processOrders initialInventory

  putStrLn $ "Order Outcomes: " ++ show results
  putStrLn $ "Final Inventory: " ++ show (M.toList finalInventory)
