module Main where

import UntypedLambda.Lib as Lib
import System.Environment

processFile :: String -> IO ()
processFile fileName = 
   Lib.parseFile fileName >>= putStr . printProgram . Lib.evalProgram

main :: IO ()
main = do
  args <- getArgs
  fileName <- case args of 
    [] -> fail "You must specify an input file"
    (x:y:_) -> fail "You must specify exactly one input file" 
    [x] -> return $ x
  processFile fileName
  
  


