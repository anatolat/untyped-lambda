module UntypedLambda.Printer (
    printTerm
  , printCommand
  , printProgram
) where

import UntypedLambda.Syntax
import UntypedLambda.Context
import Data.List (intercalate) 
import Control.Monad.State

printTerm :: Context -> Term -> String
printTerm ctx t = goTerm True ctx t
  where 
    goTerm outer ctx (TmAbs x t2) = "lambda " ++ x' ++ "." ++ sep ++ goTerm outer ctx' t2
        where (ctx', x') = pickFreshName ctx x
              isSmall = case t2 of { (TmVar _ _) -> True; _ -> False }
              sep = if isSmall && not outer then "\n" else " "
    goTerm outer ctx t = goApp outer ctx t

    goApp outer ctx (TmApp t1 t2) = goApp False ctx t1 ++ " " ++ goATerm False ctx t2
    goApp outer ctx t = goATerm outer ctx t

    goATerm outer ctx@(Context xs) (TmVar x n) =
        if contextLength ctx == n 
          then index2name ctx x
          else "[bad index: " ++ show x ++ "/" ++ show n ++ " in {" ++ show xs ++ "}]"
    goATerm outer ctx t = "(" ++ goTerm outer ctx t ++ ")"

printCommand :: Context -> Command -> String
printCommand ctx (Eval t) = printTerm ctx t
printCommand ctx (Bind x b) = x ++ " " ++ show b

runCommand :: Command -> State Context Command
runCommand cmd@(Eval _) = return cmd
runCommand cmd@(Bind x b) = modify' (\ctx -> addBinding ctx x b) >> return cmd

printProgram :: [Command] -> String
printProgram cmds = evalState (doCmds cmds) mkContext
  where 
    doCmds :: [Command] -> State Context String
    doCmds cmds = do xs <- mapM doCmd cmds
                     return $ unlines xs  

    doCmd :: Command -> State Context String
    doCmd cmd = do runCommand cmd
                   ctx <- get
                   return $ printCommand ctx cmd 