import Data.List

--read in sections of road
-- if you get stack overflows use foldl' because foldl' is strict
main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a,b,c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path -- fst :: (a, b) -> a
      pathPrice  = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice
      
-- Reverse Polish notation parser
-- Postfix

-- Not fault tolerant
-- Wait until monads to make it Maybe Float
-- because it is easier to do then
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (x * y):ys
        foldingFunction (x:y:ys) "+" = (x + y):ys
        foldingFunction (x:y:ys) "-" = (y - x):ys
        foldingFunction (x:y:ys) "/" = (y / x):ys
        foldingFunction (x:y:ys) "^" = (y ** x):ys
        foldingFunction (x:xs) "ln" = log x:xs
        foldingFunction xs "sum" = [sum xs]
        foldingFunction xs numberString = (read numberString):xs
        
-- finding shortest viable path
-- data Node = Node Road Road | EndNode Road
-- data Road = Road Int Node

--Split road into sections
        
{-|  Section
 - a - A        
       |
       c
       |
 - b - B
-}
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
--type Synonym
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]  

-- Representing the path taken
data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]  
-- Optimal pathe from Heathrow to London is 
-- [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8)] 

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA -- snd :: (a, b) -> b
      priceB = sum $ map snd pathB
      forwardPriceToA = priceA + a
      crossPriceToA   = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
      newPathToA = if forwardPriceToA <= crossPriceToA
                   then (A, a):pathA                        
                   else (C, c):(B, b):pathB
      newPathToB = if forwardPriceToB <= crossPriceToB
                   then (B, b):pathB                   
                   else (C, c):(A, a):pathA
  in (newPathToA, newPathToB)
-- roadStep ([], []) (head heathrowToLondon)

{- "Optimization tip: when we do priceA = sum $ map snd pathA, we're calculating the price from the path on every step. We wouldn't have to do that if we implemented roadStep as a (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int) function where the integers represent the best price on A and B." -}

-- solution
optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
     then reverse bestAPath
     else reverse bestBPath
          
          
groupsOf :: Int -> [a] -> [[a]]                        
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)  
             