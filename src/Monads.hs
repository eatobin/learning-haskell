-- http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

-- Conclusion
-- A functor is a data type that implements the Functor typeclass.
-- An applicative is a data type that implements the Applicative typeclass.
-- A monad is a data type that implements the Monad typeclass.
-- A Maybe implements all three, so it is a functor, an applicative, and a monad.
-- What is the difference between the three?

-- functors: you apply a function to a wrapped value using fmap or <$>
-- applicatives: you apply a wrapped function to a wrapped value using <*> or liftA
-- monads: you apply a function that returns a wrapped value, to a wrapped value using >>= or liftM

module Monads where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer.Strict
import qualified Data.Map as Map
import Data.Maybe
import System.Random

x0 = fmap (* 2) (Just 6)

x19 = fmap (* 2) Nothing

x10 = (* 2) <$> Just 6

xx3 = (* 2) <$> Nothing

x1 = liftA2 (*) (Just 5) (Just 3)

x55 = Just (* 5) <*> Just 3

z5 = Just (+ 5) <*> Just 3

x2 = fmap (* 5) (Just 3)

z = (+ 3) <$> (+ 2)

-- λ> z 6
-- 11

half :: Int -> Maybe Int
half x = if even x then Just (x `div` 2) else Nothing

-- instance Monad Maybe where
--     Nothing >>= func = Nothing
--     Just val >>= func  = func val

noNad = half 66

-- λ> noNad
-- Just 33
x3 = Just 3 >>= half

x4 = Just 4 >>= half

x5 = Nothing >>= half

x77 = liftM half (Just 34)

-- x19 = liftM half 34  Nope, doesn't compile...
xx77 = liftM half Nothing

xxx77 = liftM half (Just 33)

r5 = Just 20 >>= half >>= half >>= half

getNth :: [a] -> Int -> Maybe a
getNth [] _ = Nothing
getNth [x] _ = Just x
getNth xs i = Just (xs !! i)

randomNth :: [a] -> IO (Maybe a)
randomNth [] = return Nothing
randomNth [x] = return (Just x)
randomNth xs = Just . (xs !!) <$> randomRIO (0, length xs - 1)

-- * Monads

f = readFile "ghcid.txt" >>= putStrLn

-- f
-- ghcid --command="stack repl"

half' x = if even x then Right (x `div` 2) else Left "big error"

noNad' = half' 66

x33 = Right 3 >>= half'

x44 = Right 4 >>= half'

x65 = Left "no" >>= half'

x1920 = liftM half' (Right 4)

getNth' :: [a] -> Int -> Either String a
getNth' [] _ = Left "empty"
getNth' [x] _ = Right x
getNth' xs i = Right (xs !! i)

randomNth' :: [a] -> IO (Either String a)
randomNth' [] = return (Left "empty")
randomNth' [x] = return (Right x)
randomNth' xs = Right . (xs !!) <$> randomRIO (0, length xs - 1)

half'' :: Int -> Writer String Int
half'' x = do
  tell ("I just halved " ++ show x ++ "!")
  return (x `div` 2)

xx99 = runWriter $ half'' 8

greeter :: State String String
greeter = do
  value <- get
  put "new state!"
  return value

yy99 = runState greeter "value"

newNum :: State Int Int
newNum = do
  inputVal <- get
  put 2340
  return (inputVal + 1000)

inputVal :: Int
newState :: Int
(inputVal, newState) = runState newNum 4201

-- * Monads

-- λ> inputVal
-- 5201

-- * Monads

-- λ> newState
-- 2340

-- https://www.adit.io/posts/2013-06-10-three-useful-monads.html#the-state-monad

oldNumState :: State Int Int
oldNumState = do
  existingState <- get
  return (100 + existingState)

-- * Monads

-- λ> runState oldNumState 5
-- (105,5)

simpleNumState :: State Int Int
simpleNumState = do
  inputVal <- get
  put 42
  return inputVal

-- * Monads

-- λ> runState simpleNumState 66
-- (66,42)

-- * Monads

-- λ> execState simpleNumState 66
-- 42

-- * Monads

-- λ> evalState simpleNumState 66
-- 66

(getted, setted) = runState simpleNumState 66

newStateAfterAdding100 :: Int
oldState :: Int
(newStateAfterAdding100, oldState) = runState oldNumState 4201

-- Ok, one module loaded.

-- * Monads

-- λ> newStateAfterAdding100
-- 4301

-- * Monads

-- λ> oldState
-- 4201

-- * Monads

-- λ>

-- oldNumState :: State Int Int
-- oldNumState = do
--   put 999
--   numberToAdd <- get
--   return numberToAdd

oneMore :: State Int Int
oneMore = do
  oldState <- get
  return (oldState + 100)

-- * Monads

-- λ> runState oneMore 300
-- (400,300)

-- * Monads

-- λ> evalState oneMore 300
-- 400

-- * Monads

-- λ> execState oneMore 300
-- 300

-- * Monads

-- λ> execState oneMore (execState oneMore 300)
-- 300

-- * Monads

-- λ> evalState oneMore (execState oneMore 300)
-- 400

-- * Monads

-- λ> execState oneMore (execState oneMore 300)
-- 300

-- * Monads

-- λ> execState oneMore (evalState oneMore 300)
-- 400

-- * Monads

-- λ> evalState oneMore (evalState oneMore 300)
-- 500

data TrafficLightState = Red | Yellow | Green
  deriving (Eq, Show)

data TrafficLightAction = Stop | Slow | Go | Undefined
  deriving (Eq, Show)

goRed, goYellow, goGreen :: TrafficLightState -> (TrafficLightAction, TrafficLightState)
goGreen Red = (Go, Green)
goGreen Yellow = (Undefined, Green)
goGreen Green = (Undefined, Green)
goYellow Red = (Undefined, Yellow)
goYellow Yellow = (Undefined, Yellow)
goYellow Green = (Slow, Yellow)
goRed Red = (Undefined, Red)
goRed Yellow = (Stop, Red)
goRed Green = (Undefined, Red)

changeToGreen :: TrafficLightState -> (TrafficLightAction, TrafficLightState)
changeToGreen s0 = goGreen s0

changeToYellow :: TrafficLightState -> (TrafficLightAction, TrafficLightState)
changeToYellow s0 = goYellow s0

changeToRed :: TrafficLightState -> (TrafficLightAction, TrafficLightState)
changeToRed s0 = goRed s0

greenToRed :: TrafficLightState -> ([TrafficLightAction], TrafficLightState)
greenToRed s0 =
  let (a1, s1) = changeToYellow s0
      (a2, s2) = changeToRed s1
   in ([a1, a2], s2)

greenToRed' :: TrafficLightState -> ([TrafficLightAction], TrafficLightState)
greenToRed' s0 =
  let (a1, s1) = goYellow s0
      (a2, s2) = goRed s1
   in ([a1, a2], s2)

goRedS, goYellowS, goGreenS :: State TrafficLightState TrafficLightAction
goRedS = state goRed
goYellowS = state goYellow
goGreenS = state goGreen

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)
push Locked = (Tut, Locked)
push Unlocked = (Open, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
   in ([a1, a2, a3, a4, a5], s5)

coinS, pushS :: State TurnstileState TurnstileOutput
-- coinS :: State TurnstileState TurnstileOutput
-- pushS :: State TurnstileState TurnstileOutput
coinS = state coin
pushS = state push

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  a4 <- coinS
  a5 <- pushS
  return [a1, a2, a3, a4, a5]

but = isJust (Just 88)

nope = isJust Nothing

hereIs = fromJust (Just 66)

phoneBook = Map.fromList [(1234, "Erik"), (5678, "Patrik")]

aaa = Map.lookup 1234 phoneBook

bbb = fromMaybe "nope" aaa

vvv = fromMaybe "nope" Nothing

myMapper = Map.fromList [(1, ("Eric", "Tobin")), (2, ("Susan", "Smith"))]

mmm = Map.lookup 2 myMapper
