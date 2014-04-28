--Running sumMultipleOfThreeOrFive [1..999] gave correct answer
sumMultiplesOfThreeOrFive :: [Int] -> Int
sumMultiplesOfThreeOrFive xs = foldl (\sum x -> if x `mod` 3 == 0 || x `mod` 5 == 0 then (sum + x) else sum) 0 xs