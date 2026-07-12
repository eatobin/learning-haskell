module DrunkenPirate (runTreasureMapState, main) where

import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    execState,
    runState,
  )
import Data.Aeson (Value (String))

type Position = Int

stagger :: State Position String
stagger = do
  currentPosition <- get
  let newPosition = currentPosition + 2
  put newPosition
  return "Just staggered two"

crawl :: State Position String
crawl = do
  currentPosition <- get
  let newPosition = currentPosition + 1
  put newPosition
  return "Just crawled one"

-- runTreasureMapState :: State Position [String]
-- runTreasureMapState = do
--   msg1 <- crawl
--   msg2 <- stagger
--   msg3 <- stagger
--   return [msg1, msg2, msg3]

runTreasureMapState :: State Position [String]
runTreasureMapState = do
  let acc = [] :: [String]
  currentPosition <- get
  if currentPosition < 5
    then do
      msg <- crawl
      return (msg : acc)
    else do
      return acc

-- runTreasureMapState :: State Position [String]
-- runTreasureMapState = do
--   currentPosition <- get
--   if currentPosition < 5
--     then do
--       msg <- crawl
--       return [msg]
--     else do
--       msg <- crawl
--       let newPosition = currentPosition + 1
--       put newPosition
--       runTreasureMapState

-- gasPlus :: Int -> Int
-- gasPlus outerModule =
--   loop outerModule 0
--   where
--     loop :: Int -> Int -> Int
--     loop innerModule acc
--       | newGas <= 0 = acc
--       | otherwise = loop newGas (acc + newGas)
--       where
--         newGas = gas innerModule
-- 5 : [1, 2, 3]

main :: IO ()
main = do
  let initialPosition = 0

  print $ runState runTreasureMapState initialPosition
  print $ evalState runTreasureMapState initialPosition
  print $ execState runTreasureMapState initialPosition
