module UntypedLambda.Context (
    Context(..)
  , mkContext
  , contextLength
  , addBinding
  , addName
  , pickFreshName
  , index2name
  , name2index
) where

import Data.List
import UntypedLambda.Syntax
import Text.Printf

newtype Context = Context {unContext::[(String, Binding)]} 
                  deriving Show

mkContext :: Context
mkContext = Context []

contextLength :: Context -> Int
contextLength = length . unContext

addBinding :: Context -> String -> Binding -> Context
addBinding (Context xs) x bind = Context ((x, bind):xs)

addName :: Context -> String -> Context
addName ctx x = addBinding ctx x NameBind

isNameBound :: Context -> String -> Bool
isNameBound (Context xs) x = any (\(a,b) -> x == a) xs

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx@(Context xs) x = 
  if isNameBound ctx x 
    then pickFreshName ctx (x ++ "^") 
    else ((Context $ (x,NameBind):xs), x)

index2name :: Context -> Int -> String
index2name (Context xs) n = 
  if n < length xs 
    then fst $ xs !! n
    else error $ printf "Variable lookup failure: offset: %d, ctx size: %d" n (length xs)

name2index :: Context -> String -> Int
name2index (Context xs) x =
  case findIndex (\(a,b) -> x == a) xs of 
    Just i -> i
    Nothing -> error $ "Identifier " ++ x ++ " is unbound in {" ++ show xs ++ "}"
