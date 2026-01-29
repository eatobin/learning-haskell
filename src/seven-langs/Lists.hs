module Lists where

size :: [Int] -> Int
size [] = 0
size (h : t) = 1 + size t

prod :: [Int] -> Int
prod [] = 1
prod (h : t) = h * prod t
