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

func :: Int -> Int -> Int
func x y = 2 * x + 3 * y

flip' :: (x -> y -> z) -> y -> x -> z
flip' f = \z y -> f y z

flip'' :: (x->y->z) -> y -> x -> z
flip'' f x y = f y x
-- evens :: Integral a => [a] -> [a]
evens l = filter even l

-- evens2 :: [Integer] -> [Integer]
evens2 = filter even

square l = map (\x -> x * x) l

filterM :: (a -> Bool) -> [[a]] -> [[a]]
filterM pred matrix = map (\row -> filter pred row) matrix

filterM' :: (a -> Bool) -> [[a]] -> [[a]]
filterM' pred matrix = map (filter pred) matrix

filterM'' :: (a -> Bool) -> [[a]] -> [[a]]
filterM'' p = map (filter p)


getCol :: [[a]] -> Int -> [a]
getCol m col = map (\row -> row !! col) m

getCol' :: Int -> [[a]] -> [a]
getCol' col m = map (!! col) m

diag:: [[a]] -> [a]
diag m = map (\idx -> (m !! idx) !! idx) [0..(length m - 1)]

secDiag :: [[b]] -> [b]
secDiag m = map (\idx -> (m !! idx) !! (length m - idx - 1)) [0..(length m - 1)]

transpose :: [[a]] -> [[a]]
transpose m = map (getCol m) [0..(length m - 1)]

rotate :: [a] -> Int -> [a]
rotate l 0 = l
rotate l n
	| n < 0 || n > length l = rotate l (mod n (length l))
	| otherwise = rotate (tail l ++ [head l]) (n - 1)
