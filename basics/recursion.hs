-- Every recursive function needs an edge condition of it will never terminate

--Lets implement standard library functions!

{-| Yay ugly
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
	| x > maxTail = x
	| otherwise   = maxTail
	where maxTail = maximum' xs
-}
-- ;; beautiful
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- multiple typeclasses hoooo
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x

	   | n <= 0 = [] -- edge condition
	   | otherwise = x:replicate' (n-1) x
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = [] -- edge condition 1
take' n (x:xs) 
      | n <= 1 = [x] -- edge condition 2
      | otherwise = x:take' (n-1) xs

--reverse a list
reverse' :: [a] -> [a]
reverse' [] = [] -- edge condition
reverse' (x:xs) = reverse' xs ++ [x]

--repeat an element infinitely yay - lazy evaluation!
repeat' :: a -> [a]
repeat' a = a:repeat' a

-- Combines two lists into 2-tuples
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

--Checks if an element is in a list
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) 
      | a == x	  = True
      | otherwise = a `elem'` xs
--QUIIICK SORRT is Quick booyah
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	  let smallerSort = quicksort [a | a <- xs, a <=x]
	      biggerSort = quicksort [a | a <- xs, a > x]
	  in  smallerSort ++ [x] ++ biggerSort

{-|
	Lesson: Edge cases often happen to be identities
-}