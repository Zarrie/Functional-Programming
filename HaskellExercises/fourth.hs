import Data.List

selSort :: Ord t => [t] -> [t]
selSort [] = []
selSort list = minEl : selSort (delete minEl list)
	where minEl = minimum list

combinations :: Integer -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n list = [ (list !! i) : x | i <- [0..length list-1],
									  x <- combinations (n-1) (drop (i+1) list)]

reverse' :: [t] -> [t]
reverse' = foldr (\ x result -> result ++ [x]) []

remDup :: Eq t => [t] -> [t]
remDup = foldl helper [] where
	helper result x
		| x `elem` result = result
		| otherwise = result ++ [x]

prime :: Integral a => a -> Bool
prime 1 = False
prime n = helper (n - 1)
	where
		helper 1 = True
		helper p = mod n p /= 0 && helper (p - 1)