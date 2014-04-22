{-| Functor TypeClass |-}
import qualified Data.Map as Map
--basically for things that can be mapped over

--class Functor f where -- f is a type constructor, not a concrete type
--  fmap:: (a -> b) -> f a -> f b -- takes a functor applied with one type and returns a functor applied with another
-- Map is a functor for lists!!!!
--instance Functor [] where  
--  fmap = map
  
-- f takes a type  
--for lack of a better word, a functor can be applied to any container type (A type that can contain another type)
{-instance Functor Maybe where  
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing-}

-- functor constructor takes in another type not a concrete type

--instance Functor Tree where
--  fmap f EmptyTree = EmptyTree
--  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

--instance Functor (Either a) where  
  --fmap f (Right x) = Right (f x)  
  --fmap f (Left x) = Left x 

--instance Functor (Map.Map k) where 
  --fmap f v = f v
  
-- Type constructors take other types as parameters to eventually produce
-- Concrete types

{-| Kinds |-}

-- * Means a types is a concrete type, said as star or just type

-- Ex: Int :: *

-- Maybe :: * -> * Takes one concrete type and returns another (Int -> Maybe Int)

-- Functor :: (* -> *) -> Constraint --(* -> *) kinds can be used by Functor

class Tofu t where
  tofu :: j a -> t a j
  
data Frank a b = Frank {frankField:: b a } deriving (Show)

instance Tofu Frank where 
  tofu x = Frank x
  
data Barry t k p = Barry { yabba :: p, dabba :: t k }  -- t is (* -> *)

instance Functor (Barry a b) where  
      fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y} 

