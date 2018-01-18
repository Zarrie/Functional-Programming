hailstone :: Integer -> [Integer]
hailstone 1 = [1]
hailstone n = if even n
	then n : hailstone (n `quot` 2)
	else n : hailstone ((3 * n) + 1)


fpow :: Integer -> Integer -> Integer
fpow x 0 = 1
fpow x n = if even n
	then fpow x (n `quot` 2) * fpow x (n `quot` 2)
	else x * fpow x (n - 1)