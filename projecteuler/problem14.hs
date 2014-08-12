import Control.Monad.State.Lazy
import qualified Data.Map as M
type Length = Integer

collatz :: State (M.Map Integer Length, Integer) Length
collatz = do
  (m, x) <- get
  if x == 1
     then return 1
     else do
       case M.lookup x m of
         Just y -> return y
         Nothing -> do
            case x `mod` 2 == 0 of
              False -> do
                put (m, (3*x +1))
                val <- collatz
                (m', _) <- get
                put (M.insert x (1 + val) m', x)
                return (1 + val)
              True -> do
                put (m, (x `div` 2))
                val <- collatz
                (m', _) <- get
                put (M.insert x (1 + val) m', x)
                return (1 + val)

                       


answer = snd $ foldr (\x (m, a) -> 
                    if (evalState collatz (m, a) > evalState collatz (m, x))
                        then ((fst $ execState collatz (m, x)), a)
                        else ((fst $ execState collatz (m, x)), x))
                (M.empty, 1) [1..1000000]
