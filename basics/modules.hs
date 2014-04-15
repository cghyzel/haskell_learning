{-| Modules are collections of related functions, types, and typeclasses -}


--importing modules must be done before any functions

import Data.List -- nub, groupBy
import Data.Char -- chr, ord
import Data.Function -- on
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube 

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
     
words' :: String -> [String]     
words' = filter (not . any isSpace) . groupBy ((==) `on` isSpace)

--Caesar Cipher
encode :: Int -> String -> String
{-encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in map chr shifted-}
encode shift msg = map (chr . (+ shift) . ord) msg
     
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

-- A phone book is an associative list (dictionaries)
phone =  
  [("betty","555-2938")  
  ,("bonnie","452-2928")  
  ,("patsy","493-2928")  
  ,("lucille","205-2928")  
  ,("wendy","939-8282")  
  ,("penny","853-2492")]
  
{-findKey :: (Eq k) => k -> [(k, v)] -> v --unsafe because error when not found
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs-}
{-
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k,v):xs) = if (key == k)
                         then Just v
                         else findKey key xs-}
-- Best to do recursive list with folds
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing
-- Equivalent to lookup from Data.List
--Use Data.Map for ordering unless your keys aren't part of the Ord class

--Implementation of fromList using Map.insert
fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

phoneBook =   
  [("betty","555-2938")  
  ,("betty","342-2492")  
  ,("bonnie","452-2928")  
  ,("patsy","493-2928")  
  ,("patsy","943-2929")  
  ,("patsy","827-9162")  
  ,("lucille","205-2928")  
  ,("wendy","939-8282")  
  ,("penny","853-2492")  
  ,("penny","555-2111")  
  ]  
  --fromListWith doesn't discard duplicates and deals with pairs with the function
--phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
--phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
--makes values lists of values with a single value then concatenates them
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

