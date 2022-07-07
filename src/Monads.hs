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

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer.Strict
import           System.Random

x0 = fmap (* 2) (Just 6)
x19 = fmap (* 2) Nothing
x10 = (*2) <$> Just 6
xx3 = (*2) <$> Nothing

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
getNth []  _ = Nothing
getNth [x] _ = Just x
getNth xs  i = Just (xs !! i)

randomNth :: [a] -> IO (Maybe a)
randomNth []  = return Nothing
randomNth [x] = return (Just x)
randomNth xs  = Just . (xs !!) <$> randomRIO (0, length xs - 1)

-- *Monads
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
getNth' []  _ = Left "empty"
getNth' [x] _ = Right x
getNth' xs  i = Right (xs !! i)

randomNth' :: [a] -> IO (Either String a)
randomNth' []  = return (Left "empty")
randomNth' [x] = return (Right x)
randomNth' xs  = Right . (xs !!) <$> randomRIO (0, length xs - 1)

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

(inputVal, newState) = runState newNum 4201
-- *Monads
-- λ> inputVal
-- 5201
-- *Monads
-- λ> newState
-- 2340

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
  in ([a1, a2, a3, a4, a5], s5)

-- coinS, pushS :: State TurnstileState TurnstileOutput
coinS :: State TurnstileState TurnstileOutput
pushS :: State TurnstileState TurnstileOutput
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
