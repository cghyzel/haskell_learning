import Data.List
import Control.Monad.List
import qualified System.Random as R
import Data.Ord
import Data.Monoid
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
myReverse xs = foldr (:) [] xs

-- Problem 6
-- Find out whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = foldl (&&) True $ zipWith (==) xs $ myReverse xs 
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
removeAt _ []     = error "list too large"
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

randomNums :: Int -> Int -> IO [Int]
randomNums n m = R.getStdGen >>= return . R.randomRs (n, m)

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = (randomNums 0 (length xs -1)) >>= return . take n . foldr (\n l ->  (xs !! n):l) [] 

-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.

diff_select :: Int -> Int -> IO [Int]
diff_select n to = helper n [1..to]
    where helper n' l = do
            rs <- (randomNums 0 (length l -1))
            case n' < 1 of
              True ->   return []
              False ->  do
                remaining <- (helper (n'-1) (snd $ removeAt (i+1) l))
                return $ ((l !! i):remaining) 
                    where i = head rs

--Problem 25
-- Generate a random permutation of the elements of a list.
rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
  rs <- randomNums 0 (length xs -1)
  case null xs of
    True -> return []
    False-> do
      remaining <- rnd_permu $ snd $ removeAt (i+1) xs
      return $ (xs !! i) : (remaining)
          where i = head rs
-- Problem 26
-- Generate the combinations of K distinct objects chosen from the N elements of a list
-- tails is useful
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations 1 xs = foldr (\x result -> [x]:result) [] xs
combinations n xs = do
  i <- [1 .. length xs]
  y <- combinations (n-1) (drop i xs)
  return ((xs !! (i-1) ):y)

-- Problem 27`
-- Group the elements of a set into disjoint subsets.
-- In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.

combinations' :: Int -> Int -> [a] -> [[(a, Int)]]
combinations' 0 _ _ = [[]]
combinations' 1 m xs = foldr (\(x, n) result -> [(x, n)]:result) [] (zip xs [(m+1)..])
combinations' n m xs = do
  i <- [1 .. length xs]
  y <- combinations' (n-1) (m + i) (drop i xs)
  return ( ((xs !! (i -1) ), (i + m)):y)

removeAt' :: [Int] -> [a] -> [a]
removeAt' _ []     = error "list too large"
removeAt' (ns) (xs) = map fst $ foldr (\(x, i) result -> 
                                       if i `elem` ns 
                                       then result
                                       else (x, i):result
                                      ) [] (zip xs [1..])
group' :: [Int] -> [a] -> [[[a]]]
group' [] _ = [[]]
group' (n:ns) xs = do
  combo <- combinations' n 0 xs
  let indices = map snd combo
  let  rest = removeAt' indices xs
  otherGroups <- (group' ns rest)
  return $ (map fst combo) : otherGroups

-- Problem 28 A
-- Sorting a list of lists according to length of sublists
-- We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)
-- Problem 28 B
-- sort a list of lists according to frequency of the length
lfsort :: [[a]] -> [[a]]
lfsort ls = concat . lsort . groupBy (\as bs -> length as == length bs) $ lsort ls

-- Problem 31
-- Determine whether a given integer number is prime.
-- I had already solved this, but the haskell wiki solution is more efficient than my old solution

isPrime :: (Integral a) => a -> Bool
isPrime n | n < 4 = n > 1
isPrime n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
            where s = floor $ sqrt $ fromIntegral n
-- Problem 32
-- Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
myGCD :: Integer -> Integer -> Integer
myGCD x y =    
    case y == 0 of
      True  -> abs x
      False -> myGCD y (x `mod` y)

-- Problem 33
-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Integer -> Integer -> Bool
coprime x y= (==1) $ myGCD x y

-- Problem 34
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
totient_phi :: Integer -> Int
totient_phi m = length coprimes 
    where coprimes = do
            r <- [1..(m -1)]
            if coprime m r
            then return r
            else []

-- Problem 35
-- Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
primes :: [Integer]
primes = 2:3:[x + i | x <- [6, 12..], i <- [-1,1], isPrime (x + i)]

primeFactors :: Integer -> [Integer]
primeFactors n = helper n primes
    where helper x (p:ps)
              | x == 1         = []
              | x `mod` p == 0 = p:(helper (x `div` p) (p:ps))
              | otherwise      = helper x ps
                
-- Problem 36
-- Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.
multiplicity :: Ord a => [a] -> [(a, Int)]
multiplicity xs = map (\ys -> (head ys, length ys))  $ group $ sort xs

prime_factors_mult :: Integer -> [(Integer, Int)]
prime_factors_mult = multiplicity . primeFactors

-- Problem 37
-- Calculate Euler's totient function phi(m) (improved)
phi :: Integer -> Integer
phi a = foldr (*) 1 [ ((fromIntegral p) - 1) * ((fromIntegral p) ^ (n -1)) | (p, n) <- prime_factors_mult a]

-- Problem 39
-- A list of prime numbers.
primesR :: Integer -> Integer -> [Integer]
primesR a b = takeWhile (<= b) $ dropWhile (< a) primes

--Problem 40
-- Goldbach's conjecture.
goldbach :: Integer -> (Integer, Integer)
goldbach n = head [(p1, p2) | p1 <- ps, p2 <- ps, p1 + p2 == n]
    where ps = primesR 2 n

-- Problem 41
-- Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
goldbach_list :: Integer -> Integer -> [(Integer, Integer)]
goldbach_list n m= map goldbach . filter even $ range n m

