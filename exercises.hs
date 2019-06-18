-- Zad 48
-- span :: (a->bool)->[a]->([a],[a])
-- span _ [] = ([],[])
-- span p xs = let helper _ [] _ = ([],[])
--             helper r (y:ys) acc = 
--                 if (r y) then helper r ys (acc ++ [y])
--                 else (acc, y:ys)
--             in helper p xs []

-- Zad 49
ecd :: Eq a => [a]->[a]
ecd [] = []
ecd [x] = [x]
ecd (x1:x2:xs) = if (x1 == x2) then ecd (x2:xs)
                 else x1:ecd(x2:xs)

-- Zad 52
-- countEven :: [Int]->Int
-- countEven xs = foldr ([+].fromEnum.even) 0 xs

-- Zad 83
dataTree a  = L a | B (Tree a) (Tree a)
instance Functor Tree where
    fmap f (L a) = L(f a)
    fmap f (B l r) = B (fmap f l) (fmap f r)
instance Applicative Tree
    pure a = L a
    -- (<*>)::Tree(a -> b) -> Tree a -> Tree b
    L f <*> t - fmap f t
    B l r <*> t = B(L <*> t) (r <*> t)

1. pure id <*> r =r
2. pure (.) <*> u <*> r <*> w = u <*> (r <*> w)
3. pure f <*> pure x = pure (f x)
4. u <*> pure y = pure (?y) <*> r = pure (\x -> xy) <*> u