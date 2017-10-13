{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}

module Ch5 where

-- p 125:
-- 1.a)
-- c)

-- 1.b)
-- d)

-- 1.c)
-- b)

-- 1.d)
-- a)

-- 1.e)
-- e)

-- page 134
--Given a function and its type, tell us what type results from applying
--some or all of the arguments.
--You can check your work in the REPL like this (using the first
--question as an example):
--Prelude> let f :: a -> a -> a -> a; f = undefined
--Prelude> let x :: Char; x = undefined
--Prelude> :t f x
--It turns out that you can check the types of things that aren’t really
--implemented yet, so long as you give GHCi an undefined to bind the
--signature to.

-- λ> f1 :: a -> a -> a -> a; f1 = undefined
-- *Ch5
-- λ> :t f1
-- f1 :: a -> a -> a -> a
-- *Ch5
-- λ> :t f1 'a'
-- f1 'a' :: Char -> Char -> Char
-- 1 a

-- λ> g :: a -> b -> c -> b; g = undefined
-- *Ch5
-- λ> :t g
-- g :: a -> b -> c -> b
-- *Ch5
-- λ> :t g 0 'c' "woot"
-- g 0 'c' "woot" :: Char
-- 2 d

-- λ> h :: (Num a, Num b) => a -> b -> b; h = undefined
-- *Ch5
-- λ> :t h 1.0 2
-- h 1.0 2 :: Num b => b
-- 3 d

-- λ> h :: (Num a, Num b) => a -> b -> b; h = undefined
-- λ> :t h 1 (5.5 :: Double)
-- h 1 (5.5 :: Double) :: Double
-- 4 c

-- λ> jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
-- *Ch5
-- λ> :t jackal
-- jackal :: (Eq b, Ord a) => a -> b -> a
-- *Ch5
-- λ> :t jackal "keyboard" "has the word jackal in it"
-- jackal "keyboard" "has the word jackal in it" :: [Char]
-- 5 a

-- λ> :t jackal "keyboard"
-- jackal "keyboard" :: Eq b => b -> [Char]
-- 6 e

-- The type of 1 is Num a => a. When you pass it as the first argument to kessel,
-- its type is restricted to (Num a, Ord a) => a because kessel requires Ord for its first argument.
-- And since the type of the result is the same as that of the first argument, you get (Num a, Ord a) => a as the result type.
--
-- Note that this has nothing to do with the type of the second argument.
-- I imagine the second argument is just there to confuse you. If the function were:
--
-- kessel :: Ord a => a -> a
--
-- then the type of kessel 1 would still be (Num a, Ord a) => a.
-- The Num is there because that's the type of 1, not because of the type of kessel.
--
-- The type of kessel 'd' 2 is Char, as opposed to Ord Char => Char,
-- because Char is already a concrete type that implements Ord, so the restriction is not necessary.

-- λ> kessel :: Ord a => a -> a; kessel = undefined
-- *Ch5
-- λ> :t kessel
-- kessel :: Ord a => a -> a
-- *Ch5
-- λ> :t kessel 1
-- kessel 1 :: (Num a, Ord a) => a
-- *Ch5
-- λ> :t kessel 'v'
-- kessel 'v' :: Char
-- *Ch5
-- λ> :t kessel (1 :: Integer)
-- kessel (1 :: Integer) :: Integer
-- λ> kessel :: Ord a => a -> a; kessel x = x
-- *Ch5
-- λ> kessel 1
-- 1
-- *Ch5
-- λ> kessel 's'
-- 's'
-- *Ch5
-- λ> kessel True
-- True
-- *Ch5
-- λ> data Mood = Blah | Woot deriving Show
-- *Ch5
-- λ> :t kessel Blah
-- <interactive>:1:1: error:
--     • No instance for (Ord Mood) arising from a use of ‘kessel’
--     • In the expression: kessel Blah
-- *Ch5
-- 7 d

-- λ> :t kessel 1 (2 :: Integer)
-- kessel 1 (2 :: Integer) :: (Num a, Ord a) => a
-- *Ch5
-- 8 a

-- λ> :t kessel (1 :: Integer) 2
-- kessel (1 :: Integer) 2 :: Integer
-- *Ch5
-- 9 c

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

addTen :: Integer -> Integer
addTen = addStuff 5

fifteen :: Integer
fifteen = addTen 5

subtractStuff :: Integer -> Integer -> Integer
subtractStuff x y = x - y - 10

subtractOne :: Integer -> Integer
subtractOne = subtractStuff 1

-- λ> subtractOne 20
-- -29
-- 1 - 20 - 10

negNine :: Integer
negNine = subtractOne 0

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction ii b =
  ii + nonsense b

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (iii, bbbb) =
  iii + nonsense bbbb

anonymous :: Integer -> Bool -> Integer
anonymous =
  \ai ab -> ai + nonsense ab

anonNested :: Integer -> Bool -> Integer
anonNested =
  \aii -> \abb -> aii + nonsense abb

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry ff a b = ff (a, b)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry fff (a, b) = fff a b

sx :: Integer
sx = 5

sy :: Integer -> Integer
sy = (2^)

sz :: Integer -> Integer
sz = (^(2 :: Integer))

cv :: (Enum a, Num a, Eq a) => a -> Bool
cv = (`elem` [1..5])

hasTen :: (Num a, Eq a, Foldable t) => t a -> Bool
hasTen = elem 10

-- λ> sy sx
-- 32

-- λ> sz sx
-- 25

-- page 140

ex2a :: a -> a -> a
ex2a x _ = x

ex2b :: a -> a -> a
ex2b _ y = y

ex3 :: a -> b -> b
ex3 _ y = y

f :: Num a => a -> a -> a
f x y = x + y + 3

myConcat :: String -> String
myConcat x = x ++ " yo"

myMult :: Fractional a => a -> a
myMult x = (x / 3) * 5

myTake :: Int -> String
myTake x = take x "hey you"

myCom :: Int -> Bool
myCom x = x > length [(1 :: Integer)..10]

myAlph :: Char -> Bool
myAlph x = x < 'z'

-- page 148

-- 1 c
-- 2 a
-- 3 b
-- 4 c

example ::  Num a => a
example = 1

-- page 149


q1a :: Num a => a
q1a = (* 9) 6
-- 1a) 54 :: Num a => a
q1b :: Num t => (t, String)
q1b = head [(0,"doge"),(1,"kitteh")]
-- 1b) (0,"doge") :: Num t => (Num t, [Char])
q1c :: (Integer, String)
q1c = head [(0 :: Integer,"doge"),(1,"kitteh")]
-- 1c) (0,"doge") :: (Integer, [Char])
--q1d = if False then True else False
-- 1d) False :: Bool
q1e :: Int
q1e = length [1 :: Integer,2,3,4,5]
-- 1e) 5 :: Int
q1f :: Bool
q1f = length [1 :: Integer,2,3,4] > length "TACOCAT"
-- 1f) False :: Bool

q2x :: Num t => t
q2x = 5
q2y :: Num a => a
q2y = q2x + 5
q2w :: Num a => a
q2w = q2y * 10
-- q2w :: Num a => a

z :: Num a => a -> a
z _ = q2y * 10
-- z :: Num a => a -> a

q4f :: Fractional a => a
q4f = 4 / q2y
-- q4f :: Fractional a => a

q5x :: String
q5x = "Julie"
q5y :: String
q5y = " <3 "
q5z :: String
q5z = "Haskell"
q5f :: String
q5f = q5x ++ q5y ++ q5z
-- q5f :: String

-- bigNum = (^) 5 $ 10
bigNum :: Num a => a
bigNum = (^) 5 (10 :: Integer)
wahoo :: Num a => a
wahoo = bigNum + 10

xx :: Show a => a -> IO ()
xx = print
yy :: IO ()
yy = print "woohoo!"
zz :: IO ()
zz = xx "hello ericky"

aa :: Num a => a -> a -> a
aa = (+)
bb :: Num t => t
bb = 6
cc :: Num a => a
cc = aa bb 10
dd :: Num a => a
dd = aa cc 200

aaa :: Num a => a
aaa = 12 + bbb
bbb :: Num a => a
bbb = 10000 * ccc
ccc :: Num t => t
ccc = 4

--addEm :: Int -> Int -> Int
addEm :: Integral a => a -> a -> a
--addEm :: Fractional a => a -> a -> a
addEm x y =
  x + y

functionH :: [a] -> Maybe a
functionH []    = Nothing
functionH (x:_) = Just x

functionC :: Ord a => a -> a -> Bool
--functionC x y = if x > y then True else False
functionC x y =
  x > y

functionS :: (a, b) -> b
functionS (_, y) = y

myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) =
  (a, yToZ(xToY x))

i :: a -> a
i x =
  x

c :: a -> b -> a
c x _ =
  x

c'' :: b -> a -> b
c'' x _ =
  x

c' :: a -> b -> b
c' _ y =
  y

r :: [a] -> [a]
r []     = []
r (_:xs) = xs

co :: (b -> c) -> (a -> b) -> a -> c
co  f' g x =
  f'(g x)

a' :: (a -> c) -> a -> a
a' _ x =
  x

a'' :: (a -> b) -> a -> b
a'' f'' =
  f''


data Woot
data Blah

fink :: Woot -> Blah
fink = undefined

gimp :: (Blah, Woot) -> (Blah, Blah)
gimp (b, w) = (b, fink w)


fox :: Int -> String
fox = undefined

gorp :: String -> Char
gorp = undefined

hax :: Int -> Char
hax v = head (fox v)


data Apple
data Berry
data Chip

quo :: Apple -> Berry
quo = undefined

wad :: Berry -> Chip
wad = undefined

elf :: Apple -> Chip
elf ant = wad (quo ant)


data Xeo
data Yeo
data Zeo

xz :: Xeo -> Zeo
xz = undefined

yz :: Yeo -> Zeo
yz = undefined

xform :: (Xeo, Yeo) -> (Zeo, Zeo)
xform (xot, yot) = (xz xot, yz yot)


data Xbr
data Ybr
data Wbr
data Zbr

munge :: (Xbr -> Ybr) -> (Ybr -> (Wbr, Zbr)) -> Xbr -> Wbr
munge xty ytwaz xub = fst(ytwaz (xty xub))
