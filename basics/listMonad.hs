import Control.Applicative
--ghci> (*) <$> [1,2,3] <*> [10,100,1000]  
--[10,100,1000,20,200,2000,30,300,3000]

instance Monad [] where 
  return x = [x] -- return takes a value and wraps it in it's minimal context
  x >>= f = concat (map f xs)
  fail _ = []

-- ghci> [3,4,5] >>= \x -> [x,-x]  
-- [3,-3,4,-4,5,-5] 

-- non determinism is suppor tfor failure, empty list gets rid of error messages
{-|

ghci> [] >>= \x -> ["bad","mad","rad"]  
[]  
ghci> [1,2,3] >>= \x -> []  
[]
|-}

-- ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)  
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- previous expression do notation
listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)  

--now in list comprehension
{-|ghci> [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]  
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]  |-}


--ghci> [ x | x <- [1..50], '7' `elem` show x ]  
--[7,17,27,37,47]  

-- monadplus is a typeclass for monads that can also act as monoids

class Monad m => MonadPlus m where  
    mzero :: m a  --mempty
    mplus :: m a -> m a -> m a  --mappend

-- Lists are monoids therefore

instance MonadPlus [] where  
    mzero = []  
    mplus = (++) 

guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero 

--ghci> guard (5 > 2) >> return "cool" :: [String]  
--["cool"]  
--ghci> guard (1 > 2) >> return "cool" :: [String]  
--[]  

sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x 
{-| Knights Quest |-}

