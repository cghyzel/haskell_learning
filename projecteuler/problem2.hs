import Data.List

--solution
sumOfFibonacciEvensLessThanFourMillion :: Integer
sumOfFibonacciEvensLessThanFourMillion = sum . takeWhile(<=4000000) . filter even $ fibonacciNumbers

-- 0 and 1 are first two elemtent of fibonacci sequence
fibonacciNumbers :: [Integer]
--inefficient
--fibonacciNumbers = map fibonacciNumber [1..] 
--list comprehension
fibonacciNumbers = 1:2:[a + b | (a, b) <- zip fibonacciNumbers (tail fibonacciNumbers)]


fibonacciNumber :: Integer -> Integer
fibonacciNumber 0 = 1
fibonacciNumber 1 = 1
fibonacciNumber n = fibonacciNumber(n-1) + fibonacciNumber(n-2)
