--solution
difSumofSquareSquareOfSums :: [Integer] -> Integer
difSumofSquareSquareOfSums xs = abs (sumOfSquares xs - squareOfSum xs)

squareOfSum :: [Integer] -> Integer
squareOfSum xs = (sum xs) ^2
-- squareOfSum [1..10]
-- 3025

sumOfSquares :: [Integer] -> Integer
sumOfSquares xs = sum $ map (^2) xs
-- sumOfSquares [1..10]
-- 385
