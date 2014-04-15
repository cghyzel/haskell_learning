removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

-- [Char] is synonymous to String
removeNonUpperCase' :: String -> String
removeNonUpperCase' st = [c | c <- st, c `elem` ['A'..'Z']]

--First three are parameters
addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z

--Integer is not bounded unlike Int
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Float is a real floating point with single precision
circumference :: Float -> Float
circumference r = 2 * pi * r

-- Double is a real floating point with double the precision!
circumference' :: Double -> Double
circumference' r = 2 * pi * r 

