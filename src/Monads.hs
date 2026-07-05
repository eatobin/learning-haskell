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

import Control.Applicative ()
import Control.Monad (liftM)
import Control.Monad.Trans.State
  ( State,
    evalState,
    execState,
    get,
    put,
    runState,
    state,
  )
import Control.Monad.Trans.Writer.Strict
  ( Writer,
    runWriter,
    tell,
  )
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import System.Random (randomRIO)

x0 :: Maybe Integer
x0 = fmap (* 2) (Just 6)

x19 :: Maybe Integer
x19 = fmap (* 2) Nothing

x10 :: Maybe Integer
x10 = (* 2) <$> Just 6

xx3 :: Maybe Integer
xx3 = (* 2) <$> Nothing

x1 = liftA2 (*) (Just 5) (Just 3)

x55 = Just (* 5) <*> Just 3

z5 = Just (+ 5) <*> Just 3

x2 = fmap (* 5) (Just 3)

z :: Integer -> Integer
z = (+ 3) <$> (+ 2)

zz :: Integer -> Integer
zz = fmap (+ 3) (+ 2)

-- λ> z 6
-- 11
-- λ> zz 6
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
  return (div x 2)

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

twoMore :: State String Int
twoMore = do
  toPrint <- get
  return 888

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

data TrafficLightAction
  = IAmStoppingFromYellowToRed
  | IAmSlowingFromGreenToYellow
  | IAmGoingFromRedToGreen
  | ICannotGoFromRedToYellow
  | ICannotGoFromGreenToRed
  | ICannotGoFromYellowToGreen
  | IAmAlreadyGreen
  | IAmAlreadyYellow
  | IAmAlreadyRed
  deriving (Eq, Show)

goGreen, goYellow, goRed :: TrafficLightState -> (TrafficLightAction, TrafficLightState)
goGreen Red = (IAmGoingFromRedToGreen, Green)
goGreen Yellow = (ICannotGoFromYellowToGreen, Yellow)
goGreen Green = (IAmAlreadyGreen, Green)
goYellow Green = (IAmSlowingFromGreenToYellow, Yellow)
goYellow Red = (ICannotGoFromRedToYellow, Red)
goYellow Yellow = (IAmAlreadyYellow, Yellow)
goRed Yellow = (IAmStoppingFromYellowToRed, Red)
goRed Green = (ICannotGoFromGreenToRed, Green)
goRed Red = (IAmAlreadyRed, Red)

-- greenToRed :: TrafficLightState -> ([TrafficLightAction], TrafficLightState)
-- greenToRed s0 =
--   let (a1, s1) = goYellow s0
--       (a2, s2) = goRed s1
--    in ([a1, a2], s2)

-- greenToGreenAgain :: TrafficLightState -> ([TrafficLightAction], TrafficLightState)
-- greenToGreenAgain s0 =
--   let (a1, s1) = goYellow s0
--       (a2, s2) = goRed s1
--       (a3, s3) = goGreen s2
--    in ([a1, a2, a3], s3)

goRedState, goYellowState, goGreenState :: State TrafficLightState TrafficLightAction
goRedState = state goRed
goYellowState = state goYellow
goGreenState = state goGreen

greenToRedState :: State TrafficLightState [TrafficLightAction]
greenToRedState = do
  a1 <- goYellowState
  a2 <- goRedState
  return [a1, a2]

greenToGreenAgainState :: State TrafficLightState [TrafficLightAction]
greenToGreenAgainState = do
  a1 <- goYellowState
  a2 <- goRedState
  a3 <- goGreenState
  return [a1, a2, a3]

trafficLightMain :: IO ()
trafficLightMain =
  do
    print (runState greenToRedState Green)
    print (runState greenToRedState Yellow)
    print (runState greenToRedState Red)
    print (evalState greenToRedState Green)
    print (execState greenToRedState Green)

-- λ> trafficLightMain

-- λ> runState greenToRedState Green
-- λ> execState greenToRedState Green
-- λ> evalState greenToRedState Green

-- λ> runState goRedState Yellow
-- λ> runState goRedState Green
-- λ> runState goRedState Red

-- goRedSent, goYellowSent, goGreenSent :: TrafficLightState -> (String, TrafficLightState)
-- goGreenSent Red = ("I am red and going to green.", Green)
-- goGreenSent Yellow = ("I am yellow and can't go to green.", Yellow)
-- goGreenSent Green = ("I am green already!!", Green)
-- goYellowSent Green = ("I am green and going to yellow.", Green)
-- goYellowSent Red = ("I am red and can't go to yellow.", Red)
-- goYellowSent Yellow = ("I am yellow already!!", Yellow)
-- goRedSent Yellow = ("I am yellow and going to red.", Yellow)
-- goRedSent Green = ("I am green and can't go to red.", Green)
-- goRedSent Red = ("I am red already!!", Red)

-- goRedSentState, goYellowSentState, goGreenSentState :: State TrafficLightState String
-- goRedSentState = state goRedSent
-- goYellowSentState = state goYellowSent
-- goGreenSentState = state goGreenSent

-- greenToRedSentState :: State TrafficLightState [String]
-- greenToRedSentState = do
--   a1 <- goYellowSentState
--   a2 <- goRedSentState
--   return [a1, a2]

-- λ> runState greenToRedSentState Green
-- λ> execState greenToRedSentState Green
-- λ> evalState greenToRedSentState Green

-- λ> runState goRedSentState Yellow

-- trafficLightMain1 :: IO ()
-- trafficLightMain1 =
--   do
--     print (runState greenToRedSentState Green)
--     print (runState greenToRedSentState Yellow)
--     print (runState greenToRedSentState Red)
--     print (evalState greenToRedSentState Green)
--     print (execState greenToRedSentState Green)

-- λ> trafficLightMain1

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

but :: Bool
but = isJust (Just 88)

nope :: Bool
nope = isJust Nothing

hereIs :: Integer
hereIs = fromJust (Just 66)

phoneBook :: Map.Map Integer String
phoneBook = Map.fromList [(1234, "Erik"), (5678, "Patrik")]

aaa :: Maybe String
aaa = Map.lookup 1234 phoneBook

bbb :: String
bbb = fromMaybe "nope" aaa

vvv :: String
vvv = fromMaybe "nope" Nothing

myMapper :: Map.Map Integer (String, String)
myMapper = Map.fromList [(1, ("Eric", "Tobin")), (2, ("Susan", "Smith"))]

mmm :: Maybe (String, String)
mmm = Map.lookup 2 myMapper
