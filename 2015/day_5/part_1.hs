import Data.List

doublet :: [Char] -> Bool
doublet [x] = False
doublet (x:y:[]) = x == y
doublet (x:y:xs) = (x == y) || 
    doublet ([y] ++ xs)

noSub' :: [Char] -> [[Char]] -> Bool
noSub' x (y:[]) = isInfixOf y x
noSub' x (y:z:zs) = (isInfixOf y x) || (noSub' x ([z] ++ zs))

isVol' :: Char -> [[Char]] -> Bool
isVol' x (y:[]) = isInfixOf y [x]
isVol' x (y:ys) = (isInfixOf y [x]) || (isVol' x ys)

isVol x = isVol' x ["a","e","i","o","u"]

noSub x = not (noSub' x ["ab","cd","pq","xy"])

atLeast x = if ((length (filter (==True) (map isVol x))) 
    >= 3) then True else False 

niceString x = (doublet x) && (noSub x) && (atLeast x)

main = do
    contents <- readFile "input.txt"
    print (length (filter (==True) (map niceString (words contents))))