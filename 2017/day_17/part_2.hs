import Debug.Trace
import Data.Char
import Data.List

spin :: Int -> Int -> Int -> [Int] 
spin x y z | y == 50000000 = []
           | otherwise = 
           if (newpos == 0) 
           then [x] ++ spin (succ x) (succ y) (newpos + 1)
           else spin (succ x) (succ y) (newpos + 1)
    where newpos = (z + 343) `mod` x  

main = do
    print $ last $ spin 1 0 0
