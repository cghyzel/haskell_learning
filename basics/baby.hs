doubleme x = x+x

doubleus x y = doubleme x + doubleme y

doubleSmallNumber x = if x > 100
		  then x
		  else x*2
chris = ['c','h','r','i','s']
ghyzel = ['g','h','y','z','e','l']

chrisghyzel = chris ++ [' '] ++ ghyzel
-- ++ end of chris
-- : begining of SMALL CAT
asmallcat = 'A':" SMALL CAT"

--[] empty list

--[[]] list containing an empty list

--[1,2] same as 1:2:[] syntactic sugar baby

-- !! index accessor

numberInTheAlphabet x = "abcdefghijklmnoqrstuvwxyz" !! (x-1)

--ranges 
onethroughtwenty = [1..20]

alphabet = ['a'..'z']

--List (basically set) comprehensions

baby'sFirstListComprehension = [x*2 | x <- [1..10]]
baby'sFirstListComprehensionWithPredicate = [x*2 | x <-[1..10] , x*2 >= 12]
fiftyMod7Is3 = [x | x <- [50..100] , x `mod` 7 == 3]
--could not name this filter for the life of me
--xs is a list
boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

severalPredicates = [x | x <- [1..10], x /= 5, x /=7]

nouns = ["calvin", "pope", "Rush Limbaugh"]
adjectives = ["evil", "ignorant", "stupid"]

accurate = [adjective ++ " " ++ noun | adjective <-adjectives, noun <-nouns]

-- _ variable we don't care about
-- My length and I still haven't figured out multiple line comment yet
length' xs = sum [1 | _ <- xs]

removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

--Tuple stuff (cannot be different in lists)

twoTuples = [(1,2),(3,4),(5,6)]

--can zip infinite lists with finite lists because haskell is lazy

count = zip [1..] ["one", "two", "three", "four", "five"]
--Triangles with size <= 10
triangles = [ (a, b, c) | c <-[1..10], b <-[1..10], a <- [1..10]]
--Pythagorean theorem bitches
rightTriangles = [ (a, b, c) | c <-[1..10], b <-[1..10], a <- [1..10], a^2 + b^2 == c^2]