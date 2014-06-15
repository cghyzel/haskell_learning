{- Functor Redux -}

-- Functors are things that can be mapped over e.g. lists

-- typeclass Functor
-- one typeclass method 
-- fmap :: (a -> b) -> f a -> f b

-- Functor is computation context e.g. it contains a value or it doesn't

-- Instances of functor have a kind of  * -> *
-- Maybe Int, Maybe String

-- IO is an instance of functor
{-|
instance Functor IO where
  fmap f action = do 
    result <- action
    return (f result)
-}
-- taking advantage of fmap
import Data.Char
import Data.List

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line
          
-- (->) r
-- function type can be rewritten as -> r a
-- functions are partially applied functors

{-|
instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x)
could be written as (if the syntax allowed for it)

instance Functor (r ->) where  
    fmap f g = (\x -> f (g x))  

-}

-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)

-- with illegal syntax
          
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)

-- looks like a function composition


-- another way to write the above 
          
-- instance Functor ((->) r) where  
--    fmap = (.)

-- defined in Control.Monad.Instances
          
-- :t fmap (*3) (+100)  
-- fmap (*3) (+100) :: (Num a) => a -> a
-- 303
-- fmap closely resembles (.)

          
-- fmap :: (a -> b) -> (f a -> f b)
-- takes a function and returns a functor
-- called 'lifting a function'

{-| Functor law |-}
          
-- "The first functor law states that if we map the id function over a functor, the functor that we get back should be the same as the original functor."
-- fmap id = id
-- id can also be written as (\x -> x)

-- "The second law says that composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one."
-- fmap (f . g) = fmap f . fmap g

-- data CMaybe a = CNothing | CJust Int a deriving (Show)  
-- c stands for counter
-- Functor typeclass but not really a functor

-- instance Functor CMaybe where  
            --fmap f CNothing = CNothing  
            --fmap f (CJust counter x) = CJust (counter+1) (f x) 
-- breaks laws because of counter
-- to fix, don't increment counter   


-- with laws we can make certain assumptions
       
