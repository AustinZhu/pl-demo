module Arith.Parser (parseCode) where

import Arith.Syntax (Term (..))
import Data.Functor (($>))
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec
  ( MonadParsec (eof),
    Parsec,
    between,
    choice,
    errorBundlePretty,
    parse,
    (<|>),
  )
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

pTrue :: Parser Term
pTrue = symbol "true" $> TmTrue

pFalse :: Parser Term
pFalse = symbol "false" $> TmFalse

pInt :: Parser Term
pInt = TmInt <$> lexeme L.decimal

pConst :: Parser Term
pConst = pTrue <|> pFalse <|> pInt

pIf :: Parser Term
pIf = do
  symbol "if"
  p <- pAtom
  symbol "then"
  t1 <- pAtom
  symbol "else"
  t2 <- pAtom
  pure $ TmIf p t1 t2

pSucc :: Parser Term
pSucc = do
  symbol "succ"
  TmSucc <$> pAtom

pPred :: Parser Term
pPred = do
  symbol "pred"
  TmPred <$> pAtom

pIsZero :: Parser Term
pIsZero = do
  symbol "iszero"
  TmIsZero <$> pAtom

pTerm :: Parser Term
pTerm = choice [pConst, pIf, pSucc, pPred, pIsZero]

pAtom :: Parser Term
pAtom = pConst <|> parens pTerm

pSrc :: Parser Term
pSrc = between whitespace eof pTerm

parseCode :: String -> Term
parseCode src = case Text.Megaparsec.parse pSrc "" src of
  Left e -> error (errorBundlePretty e)
  Right t -> t
