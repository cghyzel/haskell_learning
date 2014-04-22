{- Chapter 8! -}

import qualified Data.Map as Map  

{- Algebriac Data types -}

-- data keyword defines a type

-- part after '=' are value constructors 

-- '|' is 'or'

{- data Bool = False | True -}

-- 2's complement representation

{- data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647 -}

-- Shape, defined as either a Circle or Rectangle 

-- Fields are parameters for value constructors, which are functions that 
-- return a value of a data type

-- Circle and Rectangle potential values of a shape!

-- deriving (Show) must be some sort of inheritance system, but for
-- now I know it as making Shape part of the show typeclass 

--data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- Takes a shape returns a float
-- Shapes are Circles and Rectangles
-- Circle is not an type, just like True is not a type
{-
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2 
surface (Rectangle x1 x2 y1 y2) = (abs x2 - x1) * (abs y2 - y1)
-}
-- value constructors are functions, therefore can be partially applied
-- ex map (Circle 10 20) [4.0, 5.0, 6.0] works

-- shape with intermediate type point, like a point in 2D space
-- Intermediate types make things easier to understand
data Point = Point Float Float deriving (Show)
data Shape = Rectangle Point Point | Circle Point Float deriving(Show)

surface:: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs x2 - x1) + (abs y2 - y1)

-- to move a shape, just move it's points
nudge:: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- auxilary functions! yay roman units for helping
baseCircle:: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect:: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

{-
-- FirstName LastName age height phoneNumber favoriteFlavor
data Person = Person String String Int Float String String deriving (Show)

--get separate info 
firstName:: Person -> String
firstName Person (firstName _ _ _ _ _) = firstName;


lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname    

age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height  
  
phoneNumber :: Person -> String  
phoneNumber (Person _ _ _ _ number _) = number  
  
flavor :: Person -> String  
flavor (Person _ _ _ _ _ flavor) = flavor  
-}
-- ;; the author tricked me, but I copypasted anyway so here is the better way for the same thing
-- this is called record syntax
{-|data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show) -}
                                
                                
                                
-- with record syntax, we don't have to put the fields in order, but without it we do
{-|chris :: Person -- I am a person
chris = Person {firstName="Chris", lastName="G", age=21, height=170, phoneNumber="55555", flavor="vanilla"} -}


-- In data types where position of fields is less obvious, record syntax is better

--Type constructors take types as parameters to produce new types

-- data Maybe = Nothing | Just a

-- a is the type parameter

-- because there is a type parameter, Maybe is a type constructor

--Nothing is Maybe a, it is polymorphic similarly empty list is [a]

--Don't overuse, Use it when it doesn't matter what something is

{-| !!Do not add typeclass constrains into data declarations!! |-}

-- Useless, leave optional so it is up to lower function

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m o) = (i*l) + (j*m) + (k*o)
-- typeConstructor = constructors

{-| Derived Instances |-}

-- Haskell can make our class an instance of these typeclasses with derived keyword: Eq, Ord, Enum, Bounded, Show, Read

-- Show is for things that can be converted to strings

-- Read is for things that can be converted from strings

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)

chris :: Person -- I am a person
chris = Person {firstName="Chris", lastName="G", age=21} 

-- when deriving ord, with several constructos, those values which use the first constructor are considered smaller


--data Bool = False | True deriving (Ord) therefore undo True > false

-- Nothing is less than something in Maybe!

-- when comparing two Just values, it will go further to inside them (if they are instances of ord!)

-- because constructor is are nullary (no parameters, fields) it can be part of Enum typeclass

-- Enum is for things that have predecessors and successors (pred and succ)

-- Bounded is for things that have a lowest and a highest possible value with minBound and maxBound respectively

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  deriving (Eq, Ord, Show, Read, Bounded, Enum) 

{-| Type synonyms |-}

--Doesn't really do anything, just potentially makes code more readable

--ex type String = [Char]

-- use type keyword, we're not making a new type, just a synonym
{-| Example Usage
  | type PhoneNumber = String
  | type Name = String
  | type PhoneBook = [(Name, PhoneNumber)]
  | inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
  | inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook  
  |-}

-- Type synonyms can also be parameterized

type AssocList k v = [(k, v)]
-- example getter (Eq k) => k -> AssocList k v -> Maybe v

-- Values can only have a stype that is a concrete type like String or Int

-- type IntMap v = Map Int v  is equivalent to
-- type IntMap = Map Int

-- type synonyms can only be used in the type portion of haskell

-- another data type

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)  

-- for data that can return two types ex show more detailed error messages

  
data LockerState = Taken | Free deriving (Show, Eq)  
                                             
type Code = String  
  
type LockerMap = Map.Map Int (LockerState, Code)  

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
  case Map.lookup lockerNumber map of
    Nothing  -> Left $ show lockerNumber ++ " doesn't exist"
    Just (state, code) -> if (state /= Taken)
                          then Right code
                          else Left $ show lockerNumber ++ " is taken"
                               
                               
lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
        ,(101,(Free,"JAH3I"))  
        ,(103,(Free,"IQSA9"))  
        ,(105,(Free,"QOTSA"))  
        ,(109,(Taken,"893JJ"))  
        ,(110,(Taken,"99292"))  
        ]  
    
{-| Recursive Data Structure |-}
-- A list is recursive! It contains another list
--data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
--Record syntax

--data List a = Empty | Cons { listHead :: a, listTail :: List a}
-- Cons is another word for :

--fixity declarations

--left associative
-- (4 * 3) * 2) 
-- right associative
-- (1 : (2 : (3 : Empty)))
-- right associative, tightness of binds, special character operator
infixr 5 :-:

data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- Pattern matching is actually about matching the constructors


-- Binary search tree time! (not balanced)
-- Remember a is an algebraic data structure
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleTon :: a -> Tree a
singleTon x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleTon x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x > a  = Node a left (treeInsert x right)
  | x < a  = Node a (treeInsert x left) right
             
             
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right) 
  | x == y = True
  | x <  y = treeElem x left
  | x >  y = treeElem x right
             
             
{-| Typeclasses |-}

-- Similar to interfaces
-- Defines some behavior, and classes that implement that behavior are instances of that typeclass

{-| 
  | class Eq a where -- a is the type variable (has to be a lowercase word)
  | (==) :: a -> a -> Bool
  | (/=) :: a -> a -> Bool
  | x == y = not (x /= y) -- These lines make == and /= to define each other
  | x /= y = not (x == y)
  |-}

-- Type instances help define a class like deriving

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red      = True
  Yellow == Yellow = True 
  Green == Green   = True
  _ == _           = False
  
  -- because == and /= are defined in terms of each other, 
  -- we can be lazy and only define one
  -- this is called minimum complete definition

instance Show TrafficLight where 
  show Red = "Red Light!"
  show Yellow = "Yellow Light!"
  show Green = "Green Light!"
  
  -- you can make typeclass that a subclasses of other typeclasses
  -- class (Eq a) => Num a where ...

  -- something has to be a part of eq before it can be a num

{-instance (Eq m) => Eq (Maybe m) where 
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False-}

-- :info shows what function a typeclass defines
  
  {-| YesNo typeclass |-}
-- Mimic some JavaScript behavior
class YesNo a where 
  yesno :: a -> Bool

-- Similar to C
instance YesNo Int where
  yesno 0 = False
  yesno _ = True
  
instance YesNo [a] where
  yesno [] = False
  yesno _ = True
  
instance YesNo Bool where
  yesno = id
  
instance YesNo (Maybe m) where
  yesno (Just _) = True
  yesno Nothing = False
  
instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True
  
instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True
  
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesNoVal yesResult noResult = if yesno yesNoVal then yesResult else noResult