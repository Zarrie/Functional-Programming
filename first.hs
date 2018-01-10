
addTwo :: Num a => a -> a
addTwo x = x + 2

add :: Num a => a -> a -> a
add x y = x + y

addFour :: Integer -> Integer
addFour = add 4

max' :: Ord t => t -> t -> t
max' a b = if a > b
	then a
	else b

max'' :: Ord t => t -> t -> t
max'' a b
	| a > b 	= a
	| otherwise = b

factorial :: Integer -> Integer
factorial n = if n <= 1
	then 1
	else n * factorial(n - 1)

factorial' :: Integer -> Integer
factorial' n
	| n == 1	= 1
	| otherwise = n * factorial(n - 1)

factorial'' :: Integer -> Integer
factorial'' 1 = 1
factorial'' n = n * factorial'' (n - 1)

countDigits :: (Num t, Integral t1) => t1 -> t
countDigits n = if n < 10
	then 1
	else 1 + countDigits (div n 10)

countDigits' :: (Num t, Integral t1) => t1 -> t
countDigits' n
	| n < 10	= 1
	| otherwise = 1 + countDigits'(div n 10)

prime :: Integral a => a -> Bool
prime 1 = False
prime n =
	helper (n - 1)
	where
		helper 1       = True
		helper divisor = mod n divisor /= 0 && helper (divisor - 1)

fibonacci :: Integer -> Integer
fibonacci a
	| a == 0	= 0
	| a == 1	= 1
	| otherwise = fibonacci(a - 1) + fibonacci(a - 2)