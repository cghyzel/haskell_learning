import Control.Applicative

{-|
class (Functor f) => Applicative f where  
    pure :: a -> f a -- default context
    (<*>) :: f (a -> b) -> f a -> f b 
-}

{-|
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something  
-}


-- pure f <*> x <*> y <*> ... can be written as fmap f x <*> y <*> ..
-- Applicative exports <$>, which is just an infix fmap
{-| Examples from book
ghci> Just (+3) <*> Just 9  
Just 12  
ghci> pure (+3) <*> Just 10  
Just 13  
ghci> pure (+3) <*> Just 9  
Just 12  
ghci> Just (++"hahah") <*> Nothing  
Nothing  
ghci> Nothing <*> Just "woot"  
Nothing  
ghci> pure (+) <*> Just 3 <*> Just 5   -- left associative
Just 8  
ghci> pure (+) <*> Just 3 <*> Nothing  
Nothing  
ghci> pure (+) <*> Nothing <*> Just 5  
Nothing  
-}

{-|
<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x 
-}
-- "type variables are independent of parameter names or other value names"

-- List type constructors are applicative functors
{-
instance Applicative [] where  
      pure x = [x]  
      fs <*> xs = [f x | f <- fs, x <- xs]  
-}

--Applicatives in place of list comprehensions
-- filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]

{-|
instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x) 
-}
{-|
myAction :: IO String  
myAction = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b  
-}
-- same with applicatives
-- myAction :: IO String  
-- myAction = (++) <$> getLine <*> getLine


{-|
instance Applicative ((->) r) where  
    pure x = (\_ -> x)  
    f <*> g = \x -> f x (g x)
-}

-- (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5

-- ZipList
-- Control.Applicative

{-|
instance Applicative ZipList where  
        pure x = ZipList (repeat x)  
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)  
-- Examples

ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]  
[101,102,103]  
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]  
[101,102,103]  
ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]  
[5,3,3,4]  
ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
[('d','c','r'),('o','a','a'),('g','t','t')]  

-- The (,,) function is the same as \x y z -> (x,y,z). Also, the (,) function is the same as \x y -> (x,y).
-}

--liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
--liftA2 f a b = f <$> a <*> b  

--ghci> liftA2 (:) (Just 3) (Just [4])  
--Just [3,4]  
--ghci> (:) <$> Just 3 <*> Just [4]  
--Just [3,4]  

-- sequenceA :: (Applicative f) => [f a] -> f [a]  
-- sequenceA [] = pure []  
-- sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
-- sequence with a foldr
sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA = foldr (liftA2 (:)) (pure [])

-- applicative functor "laws"
-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w) 
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u
