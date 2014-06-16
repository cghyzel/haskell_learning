import Data.Monoid
{-| Definition of monoid typeclass
class Monoid m where  
  mempty :: m  -- identity
  mappend :: m -> m -> m  -- the binary function - takes two monoids and returns a third
  mconcat :: [m] -> m  
  mconcat = foldr mappend mempty  
-}

-- associative binary function and a value that acts as an identity 

-- ex [] and ++ , 1 and *, 0 and + 

-- Monoid laws

-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- again - not enforced by haskell

-- Lists are monoids
{-| 
instance Monoid [a] where
   mempty :: []
   mappend = (++)
-}

{-|
newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded) 

instance Num a => Monoid (Product a) where  
    mempty = Product 1  
    Product x `mappend` Product y = Product (x * y)  
-}
{-| 
newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded) 
instance Monoid Any where  
        mempty = Any False  
        Any x `mappend` Any y = Any (x || y)  
-}
{-|
newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded) 
instance Monoid All where
       mempty = All True
       All x `mappend` All y = Any {x && y}
-}
{-| Compare strings using length
import Data.Monoid  
  
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (x `compare` y)  
-- with vowels
import Data.Monoid  
  
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (vowels x `compare` vowels y) `mappend`  
                    (x `compare` y)  
    where vowels = length . filter (`elem` "aeiou")  
-}

-- using monoids to fold data structures

import qualified Data.Foldable as F

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq) 

--foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m  

instance F.Foldable Tree where  
  foldMap f Empty = mempty  
  foldMap f (Node x l r) = F.foldMap f l `mappend`  
                           f x           `mappend`  
                           F.foldMap f r 
                           
testTree = Node 5  
           (Node 3  
            (Node 1 Empty Empty)  
            (Node 6 Empty Empty)  
           )  
           (Node 9  
            (Node 8 Empty Empty)  
            (Node 10 Empty Empty)  
           ) 
           
           
-- Tricks with Foldable 
-- ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree  
-- True
-- ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree  
-- False 
-- ghci> F.foldMap (\x -> [x]) testTree  
-- [1,3,6,5,8,9,10] 