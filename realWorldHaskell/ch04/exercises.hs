import Data.Char
import Data.Monoid
{-

ghci> asInt_fold "101"
101
ghci> asInt_fold "-31337"
-31337
ghci> asInt_fold "1798"
1798
-}

-- 1 Fold a number into an integer

asInt_fold numStr
  | null numStr = 0
  | head numStr == '-' = 0 -  foldr (\num acc -> acc * 10 + digitToInt num) 0 (tail numStr)
  | otherwise = foldr (\num acc -> acc * 10 + digitToInt num) 0 numStr


-- 3 Write your own version of concat using fold
myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs

-- Recursive Solution to 4
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f xs
  | null xs = [] -- we've reached the endo f the list
  | f (head xs) = (head xs): myTakeWhile f (tail xs)
  | otherwise = [] -- otherwise f is not true, which ends the recursion

myTakeWhile_fold :: (a -> Bool) -> [a] -> [a]
myTakeWhile_fold f xs = foldr acu [] xs
                      where acu x acc = if (f x) then x:acc else []

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f xs = foldl (\acc x -> if (null acc)
                                    then [[x]]
                                    else case f x (head (head acc)) of
                                      True -> (x:(head acc)) : (tail acc)
                                      False ->  [x] : acc)
                      [] xs

myAny pred = foldr (\x acc -> pred x || acc) False

myCycle xs = foldr (\_ acc -> mappend acc xs ) [] [1..]

myWords xs = let (word, lst) = foldr step ("" , []) xs
             in (if word == "" then lst else  word:lst)
                where step x acc = (if x == ' ' then (if not (fst acc == "") then ("", (fst acc):(snd acc)) else acc)else (x:fst acc, snd acc))

myUnlines :: [String] -> String
myUnlines strs = foldr (\str acc -> (str ++ "\n") `mappend` acc) "" strs
