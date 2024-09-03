module Ex5 where


mergesort :: Ord a => [a] -> [a] 
mergesort = undefined -- fixme


split :: [a] -> ([a], [a])
split xs = go ([],[]) xs
    where go :: ([a],[a]) -> [a] -> ([a],[a])
          go acc [] = acc
          go (l1,l2) (y:ys) = go (l2, y:l1) ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] l2 = l2
merge l1 [] = l1
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys