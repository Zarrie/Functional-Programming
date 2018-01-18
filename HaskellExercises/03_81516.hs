egnChecksum :: [Integer] -> Bool
egnChecksum (a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:[]) = 
	helper ((2 * a1) + (4 * a2) + (8 * a3) + (5 * a4) + (10 * a5) + (9 * a6) + (7 * a7) + (3 * a8) + (6 * a9))
		where helper n = if (n `mod` 11) < 10 then (a10 == (n `mod` 11)) else a10 == 0


sortGrades :: Num t => [[t]] -> [[t]]
sortGrades [] = []
sortGrades (x:xs) = sortGrades [y | y <- xs, length x <= length y] ++ [x] ++ sortGrades [y | y <- xs, length y < length x]


--egnChecksum [6,1,0,1,0,4,7,5,0,0] -- връща True
--egnChecksum [6,1,0,1,0,4,7,5,0,7] -- връща False

--sortGrades [[4,6,5], [2], [4,3]] -- връща [[4,6,5],[4,3],[2]]