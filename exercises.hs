-- Zad 48
span :: (a->bool)->[a]->([a],[a])
span _ [] = ([],[])
sapn p xs = let helper _ [] _ = ([],[])
            helper r (y:ys) acc = 
                if (r y) then helper r ys (acc ++ [y])
                else (acc, y:ys)
            in helper p xs []

-- Zad 49
ecd :: Eq a => [a]->[a]
ecd [] = []
ecd [x] = [x]
ecd (x1:x2:xs) = if (x1 == x2) then ecd (x2:xs)
                 else x1:ecd(x2:xs)

-- Zad 52
countEven :: [Int]->Int
countEven xs = foldr ([+].fromEnum.even) 0 xs