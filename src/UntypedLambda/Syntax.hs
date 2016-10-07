module UntypedLambda.Syntax (
    Term (..), Binding(..), Command(..)
) where

data Term = TmVar !Int !Int
          | TmAbs !String !Term
          | TmApp !Term !Term
          deriving Show

data Binding = NameBind
             deriving Show

data Command = Eval !Term
             | Bind !String !Binding
             deriving Show