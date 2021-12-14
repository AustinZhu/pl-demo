module STLC.Parser (parseCode) where

import Control.Applicative ((<**>))
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

lexeme :: Parser a -> Parser a
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

pTyArr :: Parser (STLC.Syntax.Type -> STLC.Syntax.Type)
pTyArr = do
  symbol "->"
  ty2 <- parens pTy <|> pTy
  pure (`TyArr` ty2)

pTyPair :: Parser (STLC.Syntax.Type -> STLC.Syntax.Type)
pTyPair = do
  symbol "*"
  ty2 <- parens pTy <|> pTy
  pure (`TyPair` ty2)

pTyBase :: Parser STLC.Syntax.Type
pTyBase = pTyBool <|> pTyNat <|> pTyString <|> pTyUnit

pTyAtom :: Parser STLC.Syntax.Type
pTyAtom = parens pTy <|> pTyBase

pTy :: Parser STLC.Syntax.Type
pTy = pTyAtom <**> (pTyPair <|> pTyArr <|> pure id)

pTrue :: Parser Term
pTrue = symbol "true" $> TmTrue

pFalse :: Parser Term
pFalse = symbol "false" $> TmFalse

pInt :: Parser Term
pInt = TmInt <$> lexeme L.decimal

pString :: Parser Term
pString = TmString <$> quotes (some $ C.anySingleBut '"')

pUnit :: Parser Term
pUnit = symbol "unit" $> TmUnit

pConst :: Parser Term
pConst = pTrue <|> pFalse <|> pUnit <|> pInt <|> pString

pSucc :: NameContext -> Parser Term
pSucc ctx = do
  symbol "succ"
  t <- pAtom ctx <|> pInt
  pure $ TmApp TmSucc t

pPair :: NameContext -> Parser Term
pPair ctx = do
  symbol "{"
  t1 <- pAtom ctx
  symbol ","
  t2 <- pAtom ctx
  symbol "}"
  pure $ TmPair t1 t2

pVar :: NameContext -> Parser Term
pVar ctx = do
  x <- lexeme (some C.letterChar)
  let idx = indexOf ctx x
  pure $ TmVar idx

pLam :: NameContext -> Parser Term
pLam ctx = do
  symbol "\\"
  x <- lexeme (some (C.letterChar <|> C.char '_'))
  symbol ":"
  ty <- pTy
  symbol "."
  tm <- pTerm (x : ctx)
  pure $ TmAbs x ty tm

pLet :: NameContext -> Parser Term
pLet ctx = do
  symbol "let"
  x <- lexeme (some (C.letterChar <|> C.char '_'))
  symbol "="
  tm1 <- pTerm (x : ctx)
  symbol "in"
  tm2 <- pTerm (x : ctx)
  pure $ TmLet x tm1 tm2

pApp :: NameContext -> Parser (Term -> Term)
pApp ctx = do
  C.space
  t2 <- pAtom ctx
  pure (`TmApp` t2)

pSeq :: NameContext -> Parser (Term -> Term)
pSeq ctx = do
  symbol ";"
  t2 <- pAtom ctx
  pure $ TmApp (TmAbs "_" TyUnit t2)

pFst :: NameContext -> Parser (Term -> Term)
pFst ctx = do
  symbol ".1"
  pure TmFst

pSnd :: NameContext -> Parser (Term -> Term)
pSnd ctx = do
  symbol ".2"
  pure TmSnd

pInit :: NameContext -> Parser Term
pInit ctx = pAtom ctx <**> (pSeq ctx <|> pFst ctx <|> pSnd ctx <|> pApp ctx <|> pure id)

pTerm :: NameContext -> Parser Term
pTerm ctx = pLam ctx <|> pLet ctx <|> pInit ctx <|> pSucc ctx <|> pPair ctx

pAtom :: NameContext -> Parser Term
pAtom ctx = parens (pTerm ctx) <|> pConst <|> pVar ctx

pSrc :: Parser Term
pSrc = between whitespace eof (pTerm [])

parseCode :: String -> Term
parseCode src = case parse pSrc "" src of
  Left e -> error (errorBundlePretty e)
  Right t -> t
