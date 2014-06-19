-- takes the next to last element in the list
lastButOne :: [a] -> Maybe a
lastButOne ([])     = Nothing
lastButOne (x:[])   = Nothing
lastButOne (x:y:[]) = Just x
lastButOne (x:y:xs) = lastButOne (y:xs)