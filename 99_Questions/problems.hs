import Data.List
import Control.Monad.List
-- Problem 1
-- Find the last element in a list

myLast :: [a] -> a
myLast = head . reverse

-- Problem 2
-- Find the last but one element of a list.

myButLast :: [a] -> a
myButLast = head . tail . reverse

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _     = error "Empty list"
elementAt (x:_) 1  = x
elementAt (_:xs) n = elementAt xs (n-1)

-- Problem 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength xs = foldr (\_ n -> n+1) 0 xs

-- Problem 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse xs = foldl (\r x -> x:r) [] xs

-- Problem 6
-- Find out whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = foldl (\b x -> b && x) True $ zipWith (\x y -> x==y) xs $ myReverse xs 

-- Problem 7
-- Flatten a nested list structure.

data NestedList a = Elem a | List [NestedList a]

flatten :: (NestedList a) -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List (xs))

-- Problem 8
-- Eliminate consecutive duplicates of list elements.

compress :: (Eq a) => [a] -> [a]
compress xs = foldl (\x y -> if (myLast x) == y then x else x ++ [y]) ((head xs):[]) xs

-- Problem 9 
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (sublist, rest) = span (==x) xs
              in (x:sublist) : (pack rest)
-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs =  let packed = pack xs 
             in map (\ys -> (length ys, head ys)) packed


-- Problem 11
-- Modified run-length encoding.
data Plurality a = Multiple Int a | Single a deriving (Show)

convertToPlurality :: (Int, a) -> Plurality a
convertToPlurality (x, a) 
    | x == 1 = Single a
    | x > 1  = Multiple x a

encodeModified :: (Eq a) => [a] -> [Plurality a]
encodeModified xs = let encoded = encode xs
                 in map convertToPlurality encoded

-- Problem 12
-- Decode a run-length encoded list.
convertFromPlurality :: Plurality a -> [a]
convertFromPlurality (Single a) = [a]
convertFromPlurality (Multiple x a) = replicate x a

--decodeModified :: [Plurality a] -> [a]
decodeModified = (>>= convertFromPlurality)

-- Problem 13 
-- Same as 11 

-- Problem 14
-- Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = let helper x = [x, x]
        in (>>= helper)

-- Problem 15
-- Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = (xs >>= (helper n))
    where helper m x
           | m == 1 = [x]
           | m > 1  = x:(helper (m-1) x)
-- Problem 16
-- Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs count = let helper (y:ys) n c -- number current
                             | null ys && c == 1 = []
                             | null ys           = [y]
                             | c == 1            = helper ys n n
                             | otherwise         = y:(helper ys n (c-1))
                     in helper xs count count

-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-- No predefined procedures
split :: [a] -> Int -> ([a], [a])
split [] 0     = ([], [])
split [] _     = error "invalid length of first part"
split (xs) 0 = ([], xs)
split (x:xs) n = let (first, second) = split xs (n-1)
                 in (x:first, second)

-- Problem 18
-- Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice [] _  _  = error "invalid slice"
slice xs i j =  fst $ split (snd (split xs (i -1))) (j - i + 1)

-- Problem 19
-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n = second ++ first
    where (first, second) = split xs (n `mod` (length xs))

-- Problem 20
-- Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt _ []     = error "removeAt: list too large"
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (removed, x:rest)
    where (removed, rest) = removeAt (n - 1) xs

-- Problem 21
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _     = [x]
insertAt x ys n = first ++ (x:second)
    where (first, second) = split ys (n -1)

-- Problem 22
-- Create a list containing all integers within a given range.
range :: Integer -> Integer -> [Integer]
range n m = [n..m]

-- Problem 23
-- Extract a given number of randomly selected elements from a list.

