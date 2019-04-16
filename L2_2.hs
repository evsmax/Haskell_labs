--4)
transp :: [a] -> [a]
transp n = case n of
	[] -> n
	[_] -> n
	x1:x2:xs -> x2:x1:transp xs
	
--10)
del :: Char -> String -> String
del c str = case str of
	[] -> str
	x:xs -> if x == c then del c xs
					  else x : (del c xs)