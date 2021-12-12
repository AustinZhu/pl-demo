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
    many,
    optional,
    parse,
    some,
    (<|>),
  )
import qualified Text.Megaparsec as C
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

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

pTyBool :: Parser STLC.Syntax.Type
pTyBool = symbol "Bool" $> TyBool

pTyNat :: Parser STLC.Syntax.Type
pTyNat = symbol "Nat" $> TyNat

pTyString :: Parser STLC.Syntax.Type
pTyString = symbol "String" $> TyString

pTyUnit :: Parser STLC.Syntax.Type
pTyUnit = symbol "Unit" $> TyUnit

pTyArr :: Parser STLC.Syntax.Type
pTyArr = do
  ty1 <- parens pTyArr <|> pTyBase
  arr <- pTyArr'
  pure $ arr ty1

pTyArr' :: Parser (STLC.Syntax.Type -> STLC.Syntax.Type)
pTyArr' = do
  symbol "->"
  ty2 <- parens pTy <|> pTy
  pure (`TyArr` ty2)

pTyBase :: Parser STLC.Syntax.Type
pTyBase = pTyBool <|> pTyNat <|> pTyString <|> pTyUnit

pTy :: Parser STLC.Syntax.Type
pTy = try pTyArr <|> pTyBase

pTrue :: Parser Term
pTrue = symbol "true" $> TmFalse

pFalse :: Parser Term
pFalse = symbol "false" $> TmFalse

pInt :: Parser Term
pInt = TmInt <$> L.decimal

pString :: Parser Term
pString = TmString <$> quotes (some $ C.anySingleBut '"')

pUnit :: Parser Term
pUnit = symbol "unit" $> TmUnit

pConst :: Parser Term
pConst = pTrue <|> pFalse <|> pUnit<|> pInt <|> pString

pSucc :: NameContext -> Parser Term
pSucc ctx = do
  symbol "succ"
  t <- pAtom ctx <|> pInt
  pure $ TmApp TmSucc t

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

pSeq :: NameContext -> Parser Term
pSeq ctx' = do
  t1 <- pAtom ctx'
  symbol ";"
  t2 <- pAtom ctx'
  pure $ TmApp (TmAbs "_" TyUnit t2) t1

pTerm :: NameContext -> Parser Term
pTerm ctx = try (pSeq ctx) <|> pLam ctx <|> pConst <|> pSucc ctx <|> try (pApp ctx) <|> pVar ctx

pAtom :: NameContext -> Parser Term
pAtom ctx = parens (pTerm ctx) <|> pConst <|> pVar ctx

pSrc :: Parser Term
pSrc = between whitespace eof (pTerm [])

parseCode :: String -> Term
parseCode src = case parse pSrc "" src of
  Left e -> error (errorBundlePretty e)
  Right t -> t
