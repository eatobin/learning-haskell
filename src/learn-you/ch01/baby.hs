module Learn where

doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

doubleSmallNumber x =
  if x > 100
  then x
  else x * 2

doubleSmallNumber' x =
  (if x > 100
  then x
  else x * 2)
  + 1

inThere = elem 'e' "eric"
-- True

lineUp = ['a','c'..'k']
-- "acegik"

listComp = [x * 2 | x <- [1..10]]
-- [2,4,6,8,10,12,14,16,18,20]

listComp' = [x * 2 | x <- [1..10], x * 2 >= 12]
-- [12,14,16,18,20]

listComp'' = [x | x <- [50..100], mod x 7 == 3]
-- [52,59,66,73,80,87,94]

boomBang xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]
boomBangs = boomBang [7..13]
-- ["Boom!","Boom!","Bang!","Bang!"]

notIt = [x | x <- [10..20], x /= 13, x /= 15, x /= 19]
-- [10,11,12,14,16,17,18,20]

-- to end of pg 16
