module Exercise2 where

-- text
ex0 = [True, True]
ex1 = if not (null ["hello","hola"]) then (head ["hello","hola"]) else "empty"

coll  = []
coll1 = [[], ['v','j']]
coll2 = [['h'], ['v','j']]
coll3 = ["", "Eric"]

whatIsEmpty :: [[t]] -> [Char]
whatIsEmpty []     = "Whole list is empty."
whatIsEmpty ([]:_) = "Head is empty."
whatIsEmpty (_:_)  = "Neither list nor head is empty."

ex2 = whatIsEmpty coll
ex3 = whatIsEmpty coll1
ex4 = whatIsEmpty coll2
ex5 = whatIsEmpty coll3
