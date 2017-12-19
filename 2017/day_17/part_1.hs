import Debug.Trace
import Data.Char
import Data.List
import Data.Maybe

spin :: [Int] -> Int -> Int -> [Int] 
spin x y z | y == 2017 = x
           | otherwise = spin (insert (succ y) newpos x) (succ y) (newpos + 1)
    where insert num index = (\x -> (fst (split index)) ++ [num] ++ (snd (split index)))
          split index = splitAt (index + 1) x
          newpos = (z + 343) `mod` length x

findposition ele = (\(Just i)->i) . findIndex (==ele)

main = do
    let buffer = spin [0] 0 0
    print buffer
    print (buffer !! ((findposition 2017 buffer) + 1))