--import Data.Map

duplicateElements :: [a] -> [a]
duplicateElements [] = []
duplicateElements (x:xs) = [x] ++ [x] ++ duplicateElements xs

slice :: Integer -> Integer -> [t] -> [t]
slice i j list = helper 0 list
	where
		helper m (x:xs)
			| m > j = []
			| m >= i = x : helper (m + 1) xs
			| otherwise = helper (m + 1) xs

goldbach :: Integer -> (Integer, Integer)
goldbach n = head ([(x, y) | x <- [2..n], y <- [2..n], x + y == n, prime x, prime y])
	where
		prime :: Integer -> Bool
		prime 1 = False
		prime n = helper (n - 1)
			where
				helper 1 = True
				helper divisor = mod n divisor /= 0 && helper (divisor - 1)

--type PhoneBook = Map String [String]

sumMonPairs :: [(Int, Int)] -> [Int]
sumMonPairs [] = []
sumMonPairs (x:xs) = if(fst x <= snd x)
	then (fst x + snd x) : sumMonPairs xs
	else sumMonPairs xs

digits :: [Char] -> [Char]
digits [] = []
digits (x : xs) = if ('0' <= x && x <= '9')
	then x : digits xs
	else digits xs


unzip1 :: [(a,b)] -> ([a],[b])
unzip1 [] = ([],[])
unzip1 ((x,y):xs) = (x : (fst rest), y : (snd rest))
	where rest = unzip1 xs

unzip2 :: [(a,b)] -> ([a], [b])
unzip2 list = (map fst list, map snd list)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x) : myMap f xs

timesTwo :: Integer -> Integer
timesTwo x = 2 * x

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f list = [x | x <- list, f x]

myZipWith :: (a->b->c)->[a]->[b]->[c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ _ = []

