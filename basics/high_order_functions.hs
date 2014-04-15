{-| A higher order function is a function that can take functions 
  | as parameters or return functions as return values
  |-}

{-| functions in Haskell officially only accept 1 parameter
  | so far, everything with multiple has been a curried function
  | using function application. 
  | max :: (Ord a) => a -> a -> a  is equivalent to 
  | max :: (Ord a) => a -> (a -> a)
  | Currying transforms a function that take multiple arguments such that
  | it is called as a chain of functions 
  |-}

{-| Sidenote: curried as in Haskell Curry not the food curry.
  | Haskell was fundamental to the development of Combinatory Logic
  | Combinatory logic is the foundation for functional programming
  |-}
-- Source: Wikipedia and http://learnyouahaskell.com/

-- "partially applied function takes as many parameters as is left out"

multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z  = x * y * z


-- calling a function with too few parameters creates a new function...

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x
--equivalent 
compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100
-- Compare has type (Ord a) => a -> (a-> Ordering) This is trippy

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

--More insultingly simple function
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- (-4) is negative 4, to do a partial application use (subtract 4) instead


-- Higher orderism toki ni 

applyTwice :: (a -> a) -> a -> a --parentheses here are mandatory
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--higher order functions abstract away common patterns

--flip' :: (a -> b -> c) -> (b -> a -> c) 
--flip' f = g
--      where g x y = f y x
--even more simple
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	  let smallerSorted = quicksort (filter (<=x) xs)
	      biggerSorted   = quicksort (filter (>x) xs)
	  in  smallerSorted ++ [x] ++ biggerSorted

--evaluation stops when first valid solution is found thanks to laziness
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
		 where p x = x `mod` 3829 == 0
-- Collatz sequence
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
      | even n = n:chain (n `div` 2)
      | odd n  = n:chain ((n * 3) + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
	      where isLong xs = length xs > 15	      

--Anonymous functions! \ is lambda
-- Yay lambda calculus
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- because of the way functions are curried
addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z 

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = \x y -> f y x

-- sum with fold
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- sum with fold accounting for currying
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

--foo a = bar b a can be written foo = bar b because of currying

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- map with foldr

map' :: ( a -> b ) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- map' f xs = foldl (\x acc -> acc ++ f x) [] xs is much more expensive
-- Left fold works on infinite lists!!!! tail recursion yay!
-- foldr1 and foldl1 make the first element of the list the initial value of acc
-- they don't work on empty lists though!!
--foldr is generally faster because of laziness
--Practice makes perfect
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)
 
-- scans show the state of the function
-- use takeWhile instead of filter because the list is infinite
-- takeWhile is used for ascending lists
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


-- function application
-- most of the time convenience function
-- $ is lowest precedence operator
-- equivalent to writing parenthese at the far right
{-|
 ($) :: (a -> b) -> a -> b
 f $ x = f x
-}
-- Function composition!
-- right associative
{-|
 (.) :: (b -> c) -> (a -> b) -> a -> c
 f . g = f (g x)
-}

--point free style example
--fn = ceiling . negate . tan . cos . max 50

-- point free

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- more readable

oddSquareSum' :: Integer
oddSquareSum' = 
  let oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (<10000) oddSquares
  in sum belowLimit