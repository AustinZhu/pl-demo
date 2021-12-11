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
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import UTLC.Data (Closure (..), Context, Term (..), name2Idx)
import Text.Megaparsec.Char (space1)

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
  pure $ Closure ctx (TmVar idx)

pLam :: Context -> Parser Closure
pLam ctx' = do
  symbol "\\"
  x <- lexeme (some (C.letterChar <|> C.char '_'))
  symbol "."
  cls <- try (pTerm (x : ctx')) <|> pVar (x : ctx')
  pure $ Closure (ctx cls) (TmAbs x (tm cls))

pApp :: Context -> Parser Closure
pApp ctx' = do
  cls1 <- pAtom ctx'
  cls2 <- pAtom (ctx cls1)
  pure $ Closure (ctx cls2) (TmApp (tm cls1) (tm cls2))

pTerm :: Context -> Parser Closure
pTerm ctx = pLam ctx <|> pApp ctx

pAtom :: Context -> Parser Closure
pAtom ctx = pVar ctx <|> parens (pTerm ctx)

pSrc :: Parser Closure
pSrc = between whitespace eof (pTerm [])

parseCode :: String -> Closure
parseCode src = case parse pSrc "" src of
  Left e -> error (errorBundlePretty e)
  Right t -> t
