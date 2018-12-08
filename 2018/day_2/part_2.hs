import Data.List
import Data.Ord

sames :: (String, String) -> String
sames ("","") = ""
sames (x, y) = if (head x == head y) then [head x] ++ sames tup else "" ++ sames tup
  where tup = (tail x, tail y)

--Take element and intersperse it into a list
zipper :: String -> [String] -> [(String, String)]
zipper x [y] = []
zipper x (y:ys) = [(x,y)] ++ zipper x ys

--Intersperse a list into pairs
zipper' :: [String] -> [(String, String)]
zipper' [x] = []
zipper' (x:xs) = (zipper x xs) ++ zipper' xs

main = do
  contents <- readFile "input.txt"
  let test = ["abcde", "fghij","klmno","pqrst","fguij","axcye","wvxyz"]
  let input = lines contents
  print $ maximumBy (comparing length) $ map sames $ zipper' input