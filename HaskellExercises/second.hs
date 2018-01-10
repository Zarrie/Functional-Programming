head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ []		= False
elem' x (y:ys) 	= x == y || elem x ys

sum' :: Num a => [a] -> a
sum' [] 	= 0
sum' (x:xs) = x + sum' xs

count :: (Eq a) => a -> [a] -> Integer
count _ [] = 0
count t (x:xs) = if t == x
	then 1 + count t xs
	else count t xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

union :: (Eq t) => [t] -> [t] -> [t]
union [] list = list
union (x:xs) list = if x `elem` list
	then union xs list
	else union xs ([x] ++ list)

intersection :: (Eq t) => [t] -> [t] -> [t]
intersection [] _ = []
intersection (x:xs) list = if x `elem` list
	then x : xs `intersection` list
	else xs `intersection` list

prime :: Integral a => a -> Bool
prime 1 = False
prime n =
	helper (n - 1)
	where
		helper 1 = True
		helper divisor = n `mod` divisor /= 0 && helper (divisor - 1)

difference :: (Eq t) => [t] -> [t] -> [t]
difference [] l = []
difference (x:xs) l = if x `elem` l
	then difference xs l
	else x : difference xs l

flip' :: (a->b->c) -> b -> a -> c
flip' f x y = f y x

flip'' :: (a->b->c) -> b -> a -> c
flip'' f = \x y -> f y x