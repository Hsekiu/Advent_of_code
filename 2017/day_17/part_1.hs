import Debug.Trace

spin :: [Int] -> Int -> Int -> [Int] 
spin x y z | y == 3 = x
           | otherwise = trace (show x ++ " : " ++ show newpos) $ spin (insert (succ y) newpos x) (succ y) newpos
    where insert num index = (\x -> (fst (split index)) ++ [num] ++ (snd (split index)))
          split index = splitAt (index + 1) x
          newpos = if (z + 3) < (length x) then z + 3 else (z + 3) `mod` length x