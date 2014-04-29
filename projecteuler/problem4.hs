-- just run this
largestThreeDigitProductPalindrome :: Int
largestThreeDigitProductPalindrome = foldr (max) 0 threeDigitProductPalindromes 

--long names
threeDigitProductPalindromes :: [Int]
threeDigitProductPalindromes = map read . filter (isPalindrome) $ threeDigitProducts

--easy as pie
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

--show converts to string
threeDigitProducts :: [String]
threeDigitProducts =
  let threeDigitNumbers = [100..999]
  in [show (x*y) | x <- threeDigitNumbers, y <-threeDigitNumbers]


    
    
    