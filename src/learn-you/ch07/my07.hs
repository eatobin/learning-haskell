module My07 where

data Point = Pointy Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Pointy x1 y1) (Pointy x2 y2)) =
    abs (x2 - x1) * abs (y2 - y1)

-- surface (Rectangle (Pointy 0 0) (Pointy 100 100))
-- surface (Circle (Pointy 0 0) 24)

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- λ> Person "me" "last" 44 55 "number" "choc"
-- Person {firstName = "me", lastName = "last", age = 44, height = 55.0, phoneNumber = "number", flavor = "choc"}
-- λ> :t age
-- age :: Person -> Int
-- λ> p = Person "me" "last" 44 55 "number" "choc"
-- λ> age p
-- 44
-- *My07
-- λ> p2 = p {age = 88}
-- *My07
-- λ> p2
-- Person {firstName = "me", lastName = "last", age = 88, height = 55.0, phoneNumber = "number", flavor = "choc"}
