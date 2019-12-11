import Data.List

areDoubDigits :: String -> Bool
areDoubDigits x = if (length (filter (\x -> length x == 2) (group x))) == 0 then False else True

increasingDigits :: String -> Bool
increasingDigits x
   | length x == 1 = True
   | otherwise = if (last x) >= (last list) then increasingDigits list else False
   where list = init x

main = do
   let range = [152085..670283]
   let digits = map show range
   print $ length $ filter areDoubDigits $ filter increasingDigits digits