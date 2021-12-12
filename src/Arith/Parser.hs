module Arith.Parser (parseCode) where

import Arith.Data (Term (..))
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

lexeme :: Parser String -> Parser String
lexeme = L.lexeme whitespace

symbol :: String -> Parser String
symbol = L.symbol whitespace

parens :: Parser Term -> Parser Term
parens = between (symbol "(") (symbol ")")

isKeyword :: String -> Bool
isKeyword s = s `elem` ["if", "then", "else", "true", "false", "succ", "pred", "0", "iszero", "zero"]

pTrue :: Parser Term
pTrue = do
  _ <- symbol "true"
  pure TmTrue

pFalse :: Parser Term
pFalse = do
  _ <- symbol "false"
  pure TmFalse

pZero :: Parser Term
pZero = do
  _ <- symbol "0" <|> symbol "zero"
  pure TmZero

pConst :: Parser Term
pConst = choice [pTrue, pFalse, pZero]

pIf :: Parser Term
pIf = do
  _ <- symbol "if"
  p <- pAtom
  _ <- symbol "then"
  t1 <- pAtom
  _ <- symbol "else"
  t2 <- pAtom
  pure $ TmIf p t1 t2

pSucc :: Parser Term
pSucc = do
  _ <- symbol "succ"
  TmSucc <$> pAtom

pPred :: Parser Term
pPred = do
  _ <- symbol "pred"
  TmPred <$> pAtom

pIsZero :: Parser Term
pIsZero = do
  _ <- symbol "iszero"
  TmIsZero <$> pAtom

pTerm :: Parser Term
pTerm = choice [pIf, pSucc, pPred, pIsZero]

pAtom :: Parser Term
pAtom = pConst <|> parens pTerm

pSrc :: Parser Term
pSrc = between whitespace eof pTerm

parseCode :: String -> Term
parseCode src = case Text.Megaparsec.parse pSrc "" src of
  Left e -> error (errorBundlePretty e)
  Right t -> t
