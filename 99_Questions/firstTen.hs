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
