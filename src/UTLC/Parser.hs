module UTLC.Parser (parseCode) where

import UTLC.Data (Term (..), Context, Closure (..), name2Idx)
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec
  ( MonadParsec (eof),
    Parsec,
    between,
    choice,
    errorBundlePretty,
    parse,
    (<|>), some
  )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

whitespace :: Parser ()
whitespace = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser String -> Parser String
lexeme = L.lexeme whitespace

symbol :: String -> Parser String
symbol = L.symbol whitespace

parens :: Parser Closure -> Parser Closure
parens = between (symbol "(") (symbol ")")

pVar :: Context -> Parser Closure
pVar ctx = do
  x <- lexeme (some C.letterChar)
  let idx = name2Idx ctx x
  return $ Closure ctx (TmVar idx)

pLam :: Context -> Parser Closure
pLam ctx' = do
  symbol "\\"
  x <- lexeme (some (C.letterChar <|> C.char '_'))
  symbol "."
  cls <- pTerm (x : ctx')
  return $ Closure (ctx cls) (TmAbs x (tm cls))

pApp :: Context -> Parser Closure
pApp ctx' = do
  cls1 <- parens $ pTerm ctx'
  cls2 <- parens $ pTerm (ctx cls1)
  return $ Closure (ctx cls2) (TmApp (tm cls1) (tm cls2))

pTerm :: Context -> Parser Closure
pTerm ctx = pApp ctx <|> pLam ctx <|> pVar ctx

pSrc :: Parser Closure
pSrc = between whitespace eof (pTerm [])

parseCode :: String -> Closure
parseCode src = case parse pSrc "" src of
  Left e -> error (errorBundlePretty e)
  Right t -> t
