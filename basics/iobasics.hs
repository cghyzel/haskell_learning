{-| Input and Output |-}

-- when a function changes state, we call the side-effects

-- Haskell functions can't change state by changing contents in a variable

-- putStrLn :: String -> IO ()

-- putStrLn takes a string and returns an io action that has a result type ()

{-| main = do
  | putStrLn "Hello, what's your name?"
  | name <- getLine
  | putStrLn ("Hey " ++ name ++ ", you're awesome!") -}

-- main :: IO something 

-- something is a concrete type
  
-- getLine :: IO String 

-- getLine gets the IO and returns a String bound to name

-- <- binds and removes the taint of variability

-- nameTag = "hello my name is" ++ getLine doesn't work because getLine is IO String not String/[Char]

-- IO is impure, so Haskell quarantines it

-- glue IO actions with do

import Data.Char

{-main = do
  putStrLn "Whats your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine 
  let bigFirstName = map toUpper firstName --indentation is important here
      bigLastName  = map toUpper lastName
  putStrLn $"hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"-}

-- let for pure: <- for impure (for now)

{-main = do 
  line <- getLine
  if null line
    then return () -- makes something io
    else do -- if must have then and else
         putStrLn $ reverseWords line
         main
         
reverseWords :: String -> String
reverseWords = unwords . map reverse . words -- function composition is my favorite-}

-- demonstration of return

{-main = do 
  return ()  -- these basically don't do anything
  return "HAHAHA"  
  line <- getLine  
  return "BLAH BLAH BLAH"  
  return 4  
  putStrLn line  -}

{-main = do  
  a <- return "hell"  
  b <- return "yeah!"  
  putStrLn $ a ++ " " ++ b  -} 

-- return is the opposite of <-

-- return can be sort of redundant

{-main = do
  let a = "hell"
      b = "yeah"
  putStrLn $ a ++ " " ++ b-}
          
-- putStr doesn't have a newline character
  
{- 
putStr :: String -> IO ()  
putStr [] = return ()  
putStr (x:xs) = do  
    putChar x  
    putStr xs -}

--IO still has recursion

-- print is putStrLn . show
{-
main = do   
  print True  
  print 2  
  print "haha"  
  print 3.2  
  print [3,4,3]
-}

--GHCI actually uses print to show things on terminal

-- getChar.. gets a char- wont activate till return b/c buffering

import Control.Monad   
{-  
main = do  
  c <- getChar  
  when (c /= ' ') $ do  -- if something then do some IO action else return()x
    putChar c  
    main  
  -}
  
-- sequence takes a list of IO actions and returns an IO action that will perform these actions one after another
{-
main = do  
      rs <- sequence [getLine, getLine, getLine]  
      print rs  
-}
-- mapM = sequence . map print
-- mapM_ is when we don't care about the result
-- forever... does something in an infinite loop
{-
main = forever $ do  
  putStr "Give me some input: "  
  l <- getLine  
  putStrLn $ map toUpper l 
-}


-- forM is like mapM with "parameters" inversed
-- normally used for actions defined on the spot
{-main = do   
  colors <- forM [1,2,3,4] (\a -> do 
                               putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
                              {- color <- -} getLine) -- unneccessary because <- and return just cancel each other out
--                               return color)  
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: " 
  mapM putStrLn colors  
  -}
  --if IO falls into main function they are performed
  --don't think of them are doing the action, but a function that takes a string and returns an IO action

{-| Files and Streams |-}
  
-- getContents reads everything from stdin until it encouters EOF
  
-- getContents is lazy O.o 

  {-
main = do  
  contents <- getContents  
  putStr (map toUpper contents) 
  -}
{-main = do  
  contents <- getContents  
  putStr (shortLinesOnly contents)  
    
shortLinesOnly :: String -> String  
shortLinesOnly input =   
  let allLines = lines input  
      shortLines = filter (\line -> length line < 10) allLines  
      result = unlines shortLines  
  in  result   
      -}

-- interact takes a function of type String -> String and returns an IO action that will take some input and then return that functions result as an IO action
--main = interact $ unlines . filter ((<10) . length) . lines -- composition form of shortLinesOnly

{-main = interact respondPalindromes

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
                     where isPalindrome xs = xs == reverse xs
-}
-- Now to interact with files!

-- openFile :: FilePath -> IOMode -> IO Handle
-- FilePath is a synonym for String
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode  
import System.IO

{-main = do 
  handle <- openFile "haiku.txt" ReadMode
  contents <- hGetContents handle --handle represents where the file is
  putStr contents
  hClose handle-} --hClose returns an IO action that closes the file!
  
  --hGetContents takes a Handle then gets the contents and returns an IO String
-- once again this is lazy, but we can treat contents as the whole file without running out of memory

-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
{-|
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result 
|-}
-- similar to what we did before encapsulated
{-|
main = do 
  withFile "haiku.txt" ReadMode (\handle -> do
                                    contents <- hGetContents handle
                                    putStr contents)
   -} 
-- hGetLine, hPutStr, hPutStrLn, hGetChar act like their counterparts for stdin

-- readFile :: FilePath -> IO String
{-|
main = do
       contents <-readFile "haiku.txt" --reads the files contents into an IO String 
       putStr contents
-}
-- writeFile :: FilePath -> String -> IO ()
-- truncates file!!

{-|
main = do 
  contents <- readFile "haiku.txt"
  writeFile "haikucaps.txt" (map toUpper contents)
-}
--appends instead of truncating
{-|
main = do
  todoItem <- getLine -- does not give a newline character
  appendFile "todo.txt" (todoItem ++ "\n")
-}

-- hSetBuffering takes a handle and a buffer and returns an IO action that sets the buffering 
{-|
main = do 
  withFile "haiku.txt" ReadMode (\handle -> do
                                    hSetBuffering handle  $ BlockBuffering (Just 2048)
                                    content <- hGetContents handle
                                    putStr content)
-}

--hFlush flushes the current buffer associated with a handle
-- pretty self explanatory
{-
import System.Directory
import Data.List
main = do 
  handle <- openFile "todo.txt"  ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp" --good practice to open temp file rather than make another so you don't 
  content <- hGetContents handle
  let todoTasks = lines content
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line ) [0.. ] todoTasks 
  putStrLn "These are your TO-DO items: "
  putStr $ unlines numberedTasks
  putStrLn "Which do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks  -- !! index
  hPutStr tempHandle $ unlines newTodoItems 
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"
-}
{-|
import System.Environment
import Data.List

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are: " 
  mapM putStrLn args
  putStrLn "The program name is: "
  putStrLn progName
|-}

