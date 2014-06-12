import System.Random
import Data.List
main = do
  gen <- getStdGen 
  -- getStdGen requests random number generator at the START of the progam
  -- one way to get unique strings is to continue down the infinite set
  -- other is newStdGen
  let randomChars = randomRs ('a', 'z') gen 
      (first20, rest) = splitAt 20 randomChars
      (second20, _)   = splitAt 20 rest
  putStrLn first20
  putStrLn second20
  
  