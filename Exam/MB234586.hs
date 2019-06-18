import Control.Monad
import Control.Monad.Trans.Writer.Strict

-- Zad 1

z1 n = [(a, a+1, c) | c <- [1..n], a <- [1..(c-1)], ((a^2) + ((a+1)^2)) == c^2] 

-- Zad 2

z2' n cnt acc1 acc2 | n == 0 = 0 
                    | cnt == n = acc1
                    | otherwise = z2' n (cnt+1) (acc1 + acc2 + n) acc1

z2 n = z2' n 1 0 0

-- Zad 3

krotka 0 n = []
krotka l n = [n] ++ krotka (l-1) n

z3' [] n = []
z3' (x:xs) n = ([krotka n x]) ++ (z3' xs n)

z3 lista n = z3' lista n

-- Zad 4

primeFactors' 1 _ out = out
primeFactors' n (x:xs) out | mod n x == 0 = primeFactors' (div n x) (x:xs) (out ++ [x])
                          | otherwise = primeFactors' n xs out

primeFactors n = primeFactors' n [2..n] []

fi :: Int -> Int
fi n = length(primeFactors n)

z4' n cur sum fact | cur > n = sum
                   | otherwise = z4' n (cur+1) (sum + (((fromIntegral(fi cur))/(fromIntegral newFact)))) newFact 
                        where newFact = (fact * cur)
z4 n = z4' n 1 0 1

-- Zad 5

z5 :: Int -> Writer [String] Int
z5 n = writer (perfect n, ["Got number: " ++ show n])

factors n = [a | a <- [1..n-1], mod n a == 0]

isPerfect n = (sum (factors n)) == n

perfect n = length[a | a <- [1..n], isPerfect a]