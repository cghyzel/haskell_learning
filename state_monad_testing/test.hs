import Control.Monad.State.Lazy
import qualified Data.Map as M
f :: State (M.Map Integer Integer, Integer) Integer
f = do 
  (m, x) <- get
  if x == 0 
    then return 1
    else do 
     case M.lookup x m of 
       Just y -> return y
       Nothing -> do
         put (m, x-1)
         val <- f
         (m', _) <- get
         put (M.insert x (x * val) m', x)
         return (x * val)


g :: Int -> Int
g 1 = 1
g x = x * g (x-1)

