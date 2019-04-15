z1 n = putStrLn (show(fromInteger n/1))

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = (rev xs) ++[x]

fib n = fibr n 1 0

fibr :: Integer -> Integer -> Integer -> Integer
fibr 0 v p = p
fibr n v p = fibr (n-1) (v + p) v

inits [] = [[]]
inits xs = (inits (init(xs))) ++ [xs]

ends [] = [[]]
ends (x:xs) = [[x]++xs] ++ ends(xs)

parts x = partitions [] x
partitions p [] = [[p] ++ [[]]]
partitions p (x:xs) = [[p] ++ [[x] ++ xs ]] ++ partitions (p ++ [x]) xs

inits_list l = [take n l | n <- [0..length l]]

-- trailing_zeroes n 
--     | divided == 0 = 0
--     | otherwise = divided + trailing_zeroes divided
--         where divided = (n 'div' 5)



