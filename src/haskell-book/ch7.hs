{-# OPTIONS -Wall #-}

module Ch7 where

bindExp :: Integer -> String
bindExp x =
  let y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show (y :: Integer)

bindExp' :: Integer -> String
bindExp' x =
  let y = 5 in
    let z = y + x in
      "the integer was: "
      ++ show x ++ " and y was: "
      ++ show y ++ " and z was: "
      ++ show z

-- trip :: Integer -> Integer
-- trip = \x -> x * 3
-- λ> trip 3
-- 9
-- λ> (\x -> x * 3) 5
-- 15

-- pg 224

-- mTh :: Num a => a -> a -> a -> a
-- mTh x y z = x * y * z equal to:
-- mTh = \x -> \y -> \z -> x * y * z equal to:
-- mTh = \x y z -> x * y * z

-- λ> :t mTh
-- mTh :: Num a => a -> a -> a -> a
-- λ> :t mTh 3
-- mTh 3 :: Num a => a -> a -> a

addOneIfOdd :: Integer -> Integer
addOneIfOdd n =
  if odd n then f n else n
    where f p = p + 1

addFive :: Integer -> Integer -> Integer
addFive x y = (if x > y then y else x) + 5

-- addFive' :: Integer -> Integer -> Integer
-- addFive' = \x y -> (if x > y then y else x) + 5

mflip :: (t2 -> t1 -> t) -> t1 -> t2 -> t
-- mflip f = \x -> \y -> f y x
mflip f x y =
  f y x

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

newtype Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

-- is it South Africa? If so, return True
isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

-- λ> isSouthAfrica Galapagos
-- False
-- λ> isSouthAfrica SouthAfrica
-- True

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) =
  whereitlives

-- λ> gimmeWhereTheyLive (Peng SouthAmerica)
-- SouthAmerica

humboldt :: Penguin
humboldt = Peng SouthAmerica

gentoo :: Penguin
gentoo = Peng Antarctica

macaroni :: Penguin
macaroni = Peng Antarctica

little :: Penguin
little = Peng Australia

galapagos :: Penguin
galapagos = Peng Galapagos

-- λ> gimmeWhereTheyLive little
-- Australia
-- λ> gimmeWhereTheyLive gentoo
-- Antarctica

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

-- λ> antarcticPenguin (Peng Antarctica)
-- True

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
  galapagosPenguin p ||
  antarcticPenguin p

-- λ> antarcticOrGalapagos (Peng Galapagos)
-- True
-- λ> antarcticOrGalapagos (Peng SouthAfrica)
-- False

fox :: (a, b) -> (c, d) -> ((b, d), (a, c))
fox (a, b) (c, d) = ((b, d), (a, c))

-- pg 235

k :: (a, b) -> a
k (x, _) = x

k2 :: String
k2 = k ("three", (1 :: Integer) + (2 :: Integer))

-- b) What is the type of k2? Is it the same type as k1 or k3? Nope! (Integer)

k1 :: Integer
k1 = k ((4 :: Integer) - (1 :: Integer), 10 :: Integer)

k3 :: Integer
k3 = k (3, True)

-- c) k1 & k3 = 3

fur :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
fur (a, _, c) (d, _, f) = ((a, d), (c, f))

-- funcZ :: Integer -> String
-- funcZ x =
--   case x + 1 == 1 of
--     True -> "Great!"
--     False -> "wut"

-- pal :: String -> String
-- pal xs =
--   case y of
--     True -> "yes"
--     False -> "no"
--   where
--     y = xs == reverse xs

-- pq 238

-- functionC :: Ord a => a -> a -> a
-- functionC x y =
--   case (x > y) of
--     True -> x
--     False -> y

-- ifEvenAdd2 :: Integral a => a -> a
-- ifEvenAdd2 x =
--   case even x of
--     True -> x + 2
--     False -> x

nums :: (Num a, Ord a, Integral b) => a -> b
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- λ> :t flip
-- flip :: (a -> b -> c) -> b -> a -> c
-- λ> (-) 10 1
-- 9
-- λ> flip (-) 10 1
-- -9

plusOne :: Integer -> Integer
plusOne x = x + 1

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a _ = f a

-- λ> returnAfterApply plusOne 16 6
-- 17

-- pg 242

-- data Employee =
--     Coder
--   | Manager
--   | Veep
--   | CEO
--   deriving (Eq, Ord, Show)
--
-- reportBoss :: Employee -> Employee -> IO ()
-- reportBoss e e' =
--   putStrLn $ show e ++
--     " is the boss of " ++
--     show e'
--
-- employeeRank :: Employee -> Employee -> IO ()
-- employeeRank e e' =
--   case compare e e' of
--     GT -> reportBoss e e'
--     EQ -> putStrLn "Neither employee is the boss"
--     LT -> reportBoss e' e

data Employee =
    Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++
    " is the boss of " ++
    show e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' =
  compare e e'

employeeRank :: ( Employee -> Employee -> Ordering ) -> Employee -> Employee -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> reportBoss e' e

dodgy :: Integer -> Integer -> Integer
dodgy x y = x + y + 10

oneIsOne :: (Integer -> Integer)
oneIsOne = dodgy 1

oneIsTwo :: (Integer -> Integer)
oneIsTwo = flip dodgy 2

-- λ> oneIsOne 1
-- 12
-- λ> dodgy 1 1
-- 12
-- λ> dodgy 2 2
-- 14
-- λ> dodgy 1 2
-- 13
-- λ> dodgy 2 1
-- 13
-- λ> oneIsOne 1
-- 12
-- λ> oneIsOne 2
-- 13
-- λ> oneIsTwo 1
-- 13
-- λ> oneIsTwo 2
-- 14
-- λ> oneIsOne 3
-- 14
-- λ> oneIsTwo 3
-- 15
