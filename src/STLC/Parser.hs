module STLC.Parser (parseCode) where

import qualified Data.Data as STLC
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Void (Void)
import STLC.Data (Closure (..), NameContext, Term (..), Type (..), indexOf)
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

pTyBool :: Parser STLC.Data.Type
pTyBool = symbol "Bool" $> TyBool

pTyArr :: Parser STLC.Data.Type
pTyArr = do
  ty1 <- parens pTyArr <|> pTyBool
  arr <- pTyArr'
  pure $ arr ty1

pTyArr' :: Parser (STLC.Data.Type -> STLC.Data.Type)
pTyArr' = do
  symbol "->"
  ty2 <- parens pTy <|> pTy
  pure (`TyArr` ty2)

pTy :: Parser STLC.Data.Type
pTy = try pTyArr <|> pTyBool

pTrue :: NameContext -> Parser Closure
pTrue ns = do
  symbol "true"
  pure $ Closure ns TmTrue

pFalse :: NameContext -> Parser Closure
pFalse ns = do
  symbol "false"
  pure $ Closure ns TmFalse

pConst :: NameContext -> Parser Closure
pConst ns = pTrue ns <|> pFalse ns

pVar :: NameContext -> Parser Closure
pVar ctx = do
  x <- lexeme (some C.letterChar)
  let idx = indexOf ctx x
  pure $ Closure ctx (TmVar idx)

pLam :: NameContext -> Parser Closure
pLam ctx' = do
  symbol "\\"
  x <- lexeme (some (C.letterChar <|> C.char '_'))
  symbol ":"
  ty <- pTy
  symbol "."
  cls <- try (pTerm (x : ctx')) <|> pVar (x : ctx')
  pure $ Closure (ctx cls) (TmAbs x ty (tm cls))

pApp :: NameContext -> Parser Closure
pApp ctx' = do
  cls1 <- pAtom ctx'
  cls2 <- pAtom (ctx cls1)
  pure $ Closure (ctx cls2) (TmApp (tm cls1) (tm cls2))

pTerm :: NameContext -> Parser Closure
pTerm ctx = pLam ctx <|> pConst ctx <|> pApp ctx

pAtom :: NameContext -> Parser Closure
pAtom ctx = pConst ctx <|> pVar ctx <|> parens (pTerm ctx)

pSrc :: Parser Closure
pSrc = between whitespace eof (pTerm [])

parseCode :: String -> Closure
parseCode src = case parse pSrc "" src of
  Left e -> error (errorBundlePretty e)
  Right t -> t
