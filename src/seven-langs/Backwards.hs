backwards :: [a] -> [a]
backwards [] = []
backwards (h:t) = backwards t ++ [h]
-- backwards [1,2,3] -> 1:2:3:[]
-- 2:3:[]:1
-- 3:[]:1:2
-- []:1:2:3 -> base case -> 3:2:1:[]
