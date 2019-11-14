import Data.Hash.MD5

--Determine if first 5/6 leading zero's of hash are 0's
hash :: String -> Int -> Bool
hash x y = if (take 5 (md5s (Str (x ++ show y)))) == "00000" then True else False

findNum :: String -> Int -> Int
findNum x y = if (hash x y) == False then findNum x (y + 1) else y

main = do
   print (findNum "bgvyzdsv" 0)