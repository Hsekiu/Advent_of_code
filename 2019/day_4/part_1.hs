areDoubDig :: String -> Bool
areDoubDig (x:y:xs) = if x == y then True else areDoubDig ([y] ++ xs)
areDoubDig x = False

increasingDigits :: String -> Bool
increasingDigits x
   | length x == 1 = True
   | otherwise = if (last x) >= (last list) then increasingDigits list else False
   where list = init x

main = do
   let range = [152085..670283]
   let digits = map show range
   print $ length $ filter areDoubDig $ filter increasingDigits digits