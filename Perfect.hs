-- perfect n = [a | a<-[k|k<-[1..n], sum[d|d<-[1..k], d/=k, mod k d == 0]==k]]

import Numeric 
import Control.Monad
import Control.Monad.Trans.Writer.Strict
data Natural

{-
perm [] = [[]]
perm xs = [x:ys | x <- xs, ys <- perm (remove x xs)]

remove x xs = [a | a <- xs, a /= x]
test n = [n | n <- [0..10], ]
-}

-- Cichoń 1

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = ls ++ map (x:) ls where ls = subsets xs

func1 n k = [a | a <- (subsets [1..n]), toInteger (length a) == toInteger k]

-- Cichoń 2

w :: Int -> Int
w n = length[a | a <- [2..(quot n 2)]++[n], mod n a == 0, length[x | x <- [2..floor(sqrt(fromIntegral (a-1)))], mod a x == 0] == 0]

func2 m = fromIntegral (foldl (+) 0 lista) / fromIntegral (length lista) where lista = [w a| a <- [2..m]]

-- Cichoń 3


-- data LogEntry = LogEntry { msg::String }
--   deriving (Eq, Show)

-- myGCD :: Writer [LogEntry] Integer->Writer [LogEntry] Integer-> Writer [LogEntry] Integer
-- myGCD a b | b == fromIntegral 0 = do 
--                     output (show a)
--                     return a
--           | otherwise = do 
--                     output "gcd b "
--                     output (show (mod a b))
--                     return (myGCD b (mod a b))

-- output :: String -> Writer [LogEntry] ()
-- output x = tell [LogEntry x]

-- test a b = mapM_ print $ execWriter myGCD a b