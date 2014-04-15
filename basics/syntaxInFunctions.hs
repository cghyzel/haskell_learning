-- Pattern Matching
-- Evaluates top to bottom, similar to if else if else
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Sorry you're out of luck"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- Recursion is recursive

factorial :: (Integral a) => a -> a
factorial 0 = 1;
factorial n = n * factorial(n-1)

-- Failing pattern matching (non exhaustive)

charName :: Char -> String
charName 'a' = "Alpha"
charName 'b' = "Beta"
charName 'c' = "Charlie"

-- adding tuples w/o pattern matching 

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

-- with pattern matching

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- More don't cares, for 3-tuples

first :: (a, b, c) -> a
first (x, _, _) = x
second :: (a, b, c) -> b
second (_, y, _) = y
third :: (a, b, c) -> c
third (_, _, z) = z

--Pattern match in list comprehensions

coordinates = [(1,2,3), (1,4,6), (5,4,7), (5,5,5)]
totalCoordinates = [a + b + c | (a, b, c) <- coordinates ]

-- should a pattern match fail, it will move onto the next element

--Giving head

head' :: [a] -> a
head' [] = error "Cant give no head with no list baka"
head' (x:_) = x

--TypeClass Show
tell :: (Show a) => [a] -> String
tell [] = "The list is empty bitch"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has two elements "  ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are " ++ show x ++ " and " ++ show y

--Recursive length with pattern matching and TypeClass Num
length' :: (Num b) => [a] -> b
length' [] = 0;
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--patterns yay!

capital :: String -> String
capital "" = "Empty string omg"
capital all@(s:st) = "The first letter of " ++ all ++ " is " ++ [s]

--Guards! (Patterns are for form, Guards find the truth!)
{-| Multi line comment
bmiTell :: (RealFloat a)  => a -> String
bmiTell bmi
	| bmi <= 18.5 = "You're an emo kid"
	| bmi <= 25 = "You're normal!"
	| bmi <= 30 = "You're fat!"
	| otherwise = "You're fat as fuck"
-} 
bmiTell :: (RealFloat a)  => a -> a-> String
bmiTell weight height
	| weight / height ^2 <= 18.5 = "You're an emo kid"
	| weight / height ^2 <= 25 = "You're normal!"
	| weight / height ^2 <= 30 = "You're fat!"
	| otherwise = "You're fat as fuck"

--Guards can be writtin inline too but it sucks
max' :: (Ord a) => a -> a -> a
max' x y
       | x > y     = x
       | otherwise = y

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b     = GT --Greater Than
  | a == b    = EQ --Equal
  | otherwise = LT --Less Than

-- Where in the function defines names or functions that are visible
-- within the function
bmiTell' :: (RealFloat a)  => a -> a -> String
bmiTell' weight height 
	| bmi <= skinny = "You're an emo kid"
	| bmi <= normal  = "You're normal!"
	| bmi <= fat = "You're fat!"
	| otherwise = "You're fat as fuck"
	where bmi = weight / height ^ 2  --going overboard
	      skinny = 18.5
	      normal = 25
	      fat = 30
	      -- equivalent to:
	      -- (skinny, normal, fat) = (18.5, 25, 30)
	      -- yay pattern match

--Pattern matching and where

initials :: String  -> String -> String
initials firstname lastname = [f] ++ "." ++[l] ++ "."
	where (f:_) = firstname 
	      (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
	 where bmi weight height = weight / height ^ 2

--Pattern matching and let

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
	 let sideArea = 2 * pi * r  * h
	     topArea = pi * r ^ 2
	 in sideArea + 2 * topArea
-- let <bindings> in <expression>
-- lets cannot be used across guards
-- where bindings are syntactic constructs

--Calc bmis with let and list comprehension
calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- Calc bmis with let and another predicate
calcBmis'' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0] 

--Pattern matching is syntactic sugar for case of

head'' :: [a] -> a  
head'' xs = case xs of []   -> error "No head for empty lists!"  
                       (x:_) -> x


--Cases vs where
describeList :: [a] -> String
describeList xs = case xs of [] -> "Empty!"
	     	       	     [x] -> "Singleton!"
			     xs -> "Longer!"

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
	     where what [] = "empty"
	     	   what [x] = "singleton"
		   what xs = "long"