-- tuples
fst' :: (a,b) -> a
fst' (x,y) = x

snd' :: (a, b) -> b
snd' (x,y) = y

matrixElements :: [[t]] -> [t]
matrixElements m = [m !! i !! j | i <- [0..(length m) - 1], j <- [0..(length (m !! i) - 1)]]

cartesian :: [a] -> [b] -> [(a,b)]
cartesian l1 l2 = [(a,b) | a <- l1, b <- l2]

removeAll :: (Eq t) => t -> [t] -> [t]
removeAll x l = [y | y <- l, x /= y]

count :: (Eq t) => t -> [t] -> Integer
count x l = sum [ 1 | y <- l, y == x]

histogram :: (Eq t) => [t] -> [(t, Integer)]
histogram list = [(x, (count x list)) | x <- list]

qsort :: (Ord t) => [t] -> [t]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, x < y]

naturals :: [Integer]
naturals = helper 1 where
	helper start = start : helper (start + 1)

tribonacci :: [Integer]
tribonacci = [1,1,1] ++ helper	1 1 1
	where helper a1 a2 a3 = (a1 + a2 + a3) : helper a2 a3 (a1 + a2 + a3)

sieve :: Integral a => [a]
sieve = helper [2..]
	where
		helper (x:xs) = x : helper (filter (\el -> el `mod` x > 0) xs)