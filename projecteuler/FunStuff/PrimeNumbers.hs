module FunStuff.PrimeNumbers
 (primeNumbers , primeFactorization, largestPrimeFactor) where

-- module for prime number related problems

--TODO implement Sieve of Atkins

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = head $ primeFactorization n
  
primeFactorization :: Integer -> [Integer]
primeFactorization 1 = []
primeFactorization n =
  -- take the lowest prime factor
  let factor = take 1 . filter (\p -> (n `mod` p) == 0) $ primeNumbers 
  in  primeFactorization (n `div` (head factor)) ++ factor
      --find the factors of what remains
  
-- prime numbers with sieve of euler
primeNumbers :: [Integer]
primeNumbers = eulerSieve [2..]

-- gotta love the Swiss (and Haskells laziness...)
eulerSieve :: [Integer] -> [Integer]
eulerSieve (prime:xs) =
  let nonPrimes = map (*prime) (prime:xs)
  in  prime : eulerSieve(xs `minus` nonPrimes)
  
-- taken from http://hackage.haskell.org/package/data-ordlist-0.2/docs/src/Data-List-Ordered.html
minus :: (Ord a) => [a] -> [a] -> [a]
minus = minusBy compare
  
minusBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
minusBy cmp = loop
  where
    loop [] _ys = []
    loop xs [] = xs
    loop (x:xs) (y:ys)
      = case cmp x y of
      LT -> x : loop xs (y:ys)
      EQ ->     loop xs ys
      GT ->     loop (x:xs) ys