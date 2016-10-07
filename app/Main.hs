module Main where

import UntypedLambda.Lib as Lib
import System.Environment
import System.IO

readUtf8File fileName = do
    inputHandle <- openFile fileName ReadMode 
    hSetEncoding inputHandle utf8
    hGetContents inputHandle

parseFile :: String -> IO [Command]
parseFile f = do
  content <- readUtf8File f
  return $ parseString content

processFile :: String -> IO ()
processFile fileName = 
   parseFile fileName >>= putStr . printProgram . Lib.evalProgram

main :: IO ()
main = do
  args <- getArgs
  fileName <- case args of 
    [] -> fail "You must specify an input file"
    (x:y:_) -> fail "You must specify exactly one input file" 
    [x] -> return $ x
  processFile fileName
  
  


