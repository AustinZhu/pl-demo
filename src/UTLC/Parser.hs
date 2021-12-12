module UTLC.Parser (parseCode) where

import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    Parsec,
    between,
    choice,
    errorBundlePretty,
    parse,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import UTLC.Syntax (Context, Term (..), indexOf)

type Parser = Parsec Void String

whitespace :: Parser ()
whitespace = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser String -> Parser String
lexeme = L.lexeme whitespace

symbol :: String -> Parser String
symbol = L.symbol whitespace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pVar :: Context -> Parser Term
pVar ctx = do
  x <- lexeme (some C.letterChar)
  let idx = indexOf ctx x
  pure $ TmVar idx

pLam :: Context -> Parser Term
pLam ctx' = do
  symbol "\\"
  x <- lexeme (some (C.letterChar <|> C.char '_'))
  symbol "."
  t <- pTerm (x : ctx')
  pure $ TmAbs x t

pApp :: Context -> Parser Term
pApp ctx = do
  t1 <- pAtom ctx
  t2 <- pAtom ctx
  pure $ TmApp t1 t2

pTerm :: Context -> Parser Term
pTerm ctx = pLam ctx <|> try (pApp ctx) <|> pVar ctx

pAtom :: Context -> Parser Term
pAtom ctx = parens (pTerm ctx) <|> pVar ctx

pSrc :: Parser Term
pSrc = between whitespace eof (pTerm [])

parseCode :: String -> Term
parseCode src = case parse pSrc "" src of
  Left e -> error (errorBundlePretty e)
  Right t -> t
