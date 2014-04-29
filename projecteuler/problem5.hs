-- brute force! but it works
smallestMultiple :: [Int] -> Int
smallestMultiple divisors =
  let isMultiple num = foldl (\acc isMult -> if isMult then acc else False) True $ map (\divisor -> if num `mod` divisor == 0 then True else False) divisors
  in head . take 1 . filter(isMultiple) $ [1..]