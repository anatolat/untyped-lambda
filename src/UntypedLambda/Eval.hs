module UntypedLambda.Eval (
    isval
  , evalTerm
  , evalProgram
) where

import UntypedLambda.Syntax
import UntypedLambda.Context
import Control.Monad.State

isval ::  Term -> Bool
isval (TmAbs _ _) = True
isval _ = False

evalTerm :: Context -> Term -> Term
evalTerm ctx t = maybe t (evalTerm ctx) (eval1 ctx t)
  where
    eval1 ctx t = case t of
      TmApp (TmAbs x t12) v2 | isval v2 -> Just $ termSubstTop v2 t12
      TmApp v1 t2 | isval v1 -> TmApp v1 <$> eval1 ctx t2
      TmApp t1 t2 -> TmApp <$> eval1 ctx t1 <*> pure t2
      _ -> Nothing 

evalCommand :: Command -> State Context Command
evalCommand cmd@(Eval t) = 
  do ctx <- get 
     return $ Eval $ evalTerm ctx t
evalCommand cmd@(Bind x b) = 
  do modify' (\ctx -> addBinding ctx x b) 
     return cmd

evalProgram :: [Command] -> [Command]
evalProgram cmds = evalState (mapM evalCommand cmds) mkContext

-- Shifting
tmmap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmmap onvar c t = walk c t
  where
    walk c (TmVar x n)  = onvar c x n
    walk c (TmAbs x t2) = TmAbs x (walk (c + 1) t2)
    walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d c t =
    tmmap (\c x n -> if x >= c then TmVar (x+d) (n+d) else TmVar x (n+d)) c t

termShift :: Int -> Term -> Term
termShift d t = termShiftAbove d 0 t

-- Substitution
termSubst :: Int -> Term -> Term -> Term
termSubst j s t = tmmap (\c x n -> if x==j+c then termShift c s else TmVar x n) 0 t

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) $ termSubst 0 (termShift 1 s) t