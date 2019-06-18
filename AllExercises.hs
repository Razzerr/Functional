-- Load this file into Haskell using > :load AllExercises.hs

---- Zad 3
-- Part 1
euler_in n m i  | m == 0 = i
                | gcd n m == 1 = euler_in n (m-1) (i+1)
                | otherwise = euler_in n (m-1) i
euler_rec n = euler_in n n 0

----

euler_lst n = length[a | a <- [1..n], gcd a n == 1]

-- Part 2
euler_sum n = sum[euler_lst a | a <- [1..n], mod n a == 0]

---- Zad 4
pit_triple = [(a, b, c) | a <- [1..200], b <- [1..a], c <- [1..b], a^2 == b^2 + c^2, gcd b c == 1]

---- Zad 5

fib x | x == 0 = 0
      | x == 1 = 1
      | otherwise = fib (x-1) + fib(x-2)

betterFibIn x cnt acc1 acc2 | x == 0 = 0 
                            | cnt == x = acc1
                            | otherwise = betterFibIn x (cnt+1) (acc1 + acc2) acc1

betterFib x = betterFibIn x 1 1 0

-- Quicksort
iteration [] pivot lessEq more = [lessEq, more]
iteration (x:xs) pivot lessEq more  | x <= pivot = iteration xs pivot (lessEq ++ [x]) more
                                    | otherwise = iteration xs pivot lessEq (more ++ [x])

filterIn op [] out = out
filterIn op (x:xs) out | (op x) = filterIn (op) xs (out ++ [x])
                       | otherwise = filterIn (op) xs out
myFilter op list = filterIn (op) list [] 

-- myFilter (> 3) [1, 2, 5, 7, 4]

quicksort [] = []
quicksort (h:t) = do
    let (l:(m:p)) = iteration t h [] []
    (quicksort l) ++ [h] ++ (quicksort m)

sitoIn [] out = out
sitoIn (x:xs) out = sitoIn [a | a <- xs, mod a x /= 0] (out ++ [x])  

-- sito n = [a | a <- [2..n], ]

secondEven' [] last = []
secondEven' (x:xs) last  | ((mod x 2 == 0) && last) = secondEven' xs False
                        | mod x 2 == 0 = [x] ++ secondEven' xs True
                        | otherwise = [x] ++ secondEven' xs last

secondEven list = secondEven' list False

secondEven2' [] = []
secondEven2' (x:(d:xs))  | (mod x 2 == 0) && (mod d 2 == 0) = [x] ++ secondEven2' xs
                         | otherwise = [x] ++ secondEven2' (d:xs)

                         
            
elIdx n [] id = -1
elIdx n (x:xs) id | x == n = id
                    | otherwise = elIdx n xs (id+1) 

getEl n (x:xs) id | n == id = x
                    | id > n = -1
                    | otherwise = getEl n xs (id+1)

fact 1 = []
fact n = [curFact] ++ (fact (n `div` curFact)) 
    where curFact = (getEl (elIdx 0 (map (mod n) ([2..(n `div` 2)] ++ [n])) 0) ([2..(n `div` 2)] ++ [n]) 0)



