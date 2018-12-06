import Data.List
import Data.Set as Set hiding (map)
import Debug.Trace

--trace' arg = traceShow arg arg

toInt :: String -> Int
toInt (x:xs) = if x == '-' then (-1) * read xs :: Int else read xs :: Int

--Keep a history of freq that have been visted run through input till double occurence
twofreq :: [Int] -> Int -> Set Int -> Int -> Int
twofreq nums pos his freq = 
  if member calc his then calc 
  else twofreq nums newpos (Set.insert calc his) calc
  where calc = (nums !! pos) + freq
        newpos = if length nums - 1 == pos then 0 else pos + 1

main = do
  contents <- readFile "input.txt"
  let input = lines contents
  let ans = twofreq (map toInt input) 0 (singleton 0) 0
  print ans