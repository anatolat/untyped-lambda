module UntypedLambda.Parser (
    toplevel
  , parseString
  , parseFile
) where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import UntypedLambda.Syntax
import UntypedLambda.Context

type Parser = Parsec String Context

lexer = Token.makeTokenParser langDef
  where 
    langDef = emptyDef 
      { Token.identStart = letter
      , Token.identLetter = alphaNum
      , Token.reservedNames = [ "lambda" ]
      , Token.commentStart = "/*"
      , Token.commentEnd = "*/"
      }

lcid = Token.identifier lexer
lambda = Token.reserved lexer "lambda"
semi = Token.semi lexer
dot = Token.dot lexer

parseString :: String -> [Command]
parseString str = case runParser toplevel mkContext "" str of
  Left e -> error $ show e
  Right t -> t

parseFile :: String -> IO [Command]
parseFile f = do
  content <- readFile f
  case runParser toplevel mkContext f content of
    Left e -> fail $ show e
    Right t -> return t

toplevel :: Parser [Command]
toplevel = Token.whiteSpace lexer *> endBy command semi <* eof 

command :: Parser Command
command = try parseBinding <|> Eval <$> term
  where 
    parseBinding = 
      do id <- lcid 
         b <- binder
         updateState $ \ctx -> addName ctx id
         return $ Bind id b

binder :: Parser Binding
binder = (pure NameBind <* Token.symbol lexer "/") 

term :: Parser Term
term = appTerm
     <|> do lambda
            id <- lcid <|> Token.symbol lexer "_"
            dot
            ctx <- getState
            putState $ addName ctx id 
            t <- term
            putState ctx
            return $ TmAbs id t

appTerm :: Parser Term
appTerm = chainl1 aterm (return TmApp)

aterm :: Parser Term
aterm = Token.parens lexer term
      <|> do id <- lcid 
             ctx <- getState 
             return $ TmVar (name2index ctx id) (contextLength ctx)