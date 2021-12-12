module STLC.Parser (parseCode) where

import qualified Data.Data as STLC
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Void (Void)
import STLC.Syntax (NameContext, Term (..), Type (..), indexOf)
import System.Exit (exitFailure)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    Parsec,
    between,
    choice,
    errorBundlePretty,
    optional,
    parse,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

whitespace :: Parser ()
whitespace = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser String -> Parser String
lexeme = L.lexeme whitespace

symbol :: String -> Parser String
symbol = L.symbol whitespace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTyBool :: Parser STLC.Syntax.Type
pTyBool = symbol "Bool" $> TyBool

pTyArr :: Parser STLC.Syntax.Type
pTyArr = do
  ty1 <- parens pTyArr <|> pTyBool
  arr <- pTyArr'
  pure $ arr ty1

pTyArr' :: Parser (STLC.Syntax.Type -> STLC.Syntax.Type)
pTyArr' = do
  symbol "->"
  ty2 <- parens pTy <|> pTy
  pure (`TyArr` ty2)

pTy :: Parser STLC.Syntax.Type
pTy = try pTyArr <|> pTyBool

pTrue :: Parser Term
pTrue = do
  symbol "true"
  pure TmTrue

pFalse :: Parser Term
pFalse = do
  symbol "false"
  pure TmFalse

pConst :: Parser Term
pConst = pTrue <|> pFalse

pVar :: NameContext -> Parser Term
pVar ctx = do
  x <- lexeme (some C.letterChar)
  let idx = indexOf ctx x
  pure $ TmVar idx

pLam :: NameContext -> Parser Term
pLam ctx' = do
  symbol "\\"
  x <- lexeme (some (C.letterChar <|> C.char '_'))
  symbol ":"
  ty <- pTy
  symbol "."
  tm <- pTerm (x : ctx')
  pure $ TmAbs x ty tm

pApp :: NameContext -> Parser Term
pApp ctx' = do
  t1 <- pAtom ctx'
  t2 <- pAtom ctx'
  pure $ TmApp t1 t2

pTerm :: NameContext -> Parser Term
pTerm ctx = pLam ctx <|> pConst <|> try (pApp ctx) <|> pVar ctx

pAtom :: NameContext -> Parser Term
pAtom ctx = parens (pTerm ctx) <|> pConst <|> pVar ctx

pSrc :: Parser Term
pSrc = between whitespace eof (pTerm [])

parseCode :: String -> Term
parseCode src = case parse pSrc "" src of
  Left e -> error (errorBundlePretty e)
  Right t -> t
