import System.Environment  
import System.IO  
import System.IO.Error

  
  
--exceptions in IO

{-main = do (fileName:_) <- getArgs  
          fileExists <- doesFileExist fileName
          if fileExists
            then do contents <- readFile fileName  
                    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
            else do putStrLn "The file does not exist"
-}

-- catch ::  IO a -> (IOError -> IO a) -> IO a
-- catch :: IO action -> Exception hander -> IO action

main = toTry `catch` handler

toTry :: IO()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO()
handler e 
  | isDoesNotExistError e = 
    case ioeGetFileName e of 
      Just path -> putStrLn $ "Whoops, file does not exist at " ++ path
      Nothing -> putStrLn "Whoops, file does not exist at unknown location"
    
  | otherwise = ioError e
                
-- IOError predicates 
-- isAlreadyExistsError
-- isDoesNotExistError
-- isAlreadyInUseError
-- isFullError
-- isEOFError
-- isIllegalOperation
-- isPermissionError
--isUserError

-- full list of errors: http://www.haskell.org/ghc/docs/6.10.1/html/libraries/base/System-IO-Error.html#3

