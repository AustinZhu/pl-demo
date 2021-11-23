module ArithmeticExpr.Parser where

import ArithmeticExpr.Data
import Data.Void
import System.Exit (exitFailure)
import Text.Megaparsec
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
isKeyword s = s `elem` ["if", "then", "else", "True", "False", "succ", "pred", "0", "isZero", "zero"]

pIf :: Parser Term
pIf = do
  _ <- symbol "if"
  p <- pConst <|> parens pTerm
  _ <- symbol "then"
  t1 <- pConst <|> parens pTerm
  _ <- symbol "else"
  t2 <- pConst <|> parens pTerm
  pure $ TmIf p t1 t2

pTrue :: Parser Term
pTrue = do
  _ <- symbol "True"
  pure TmTrue

pFalse :: Parser Term
pFalse = do
  _ <- symbol "False"
  pure TmFalse

pSucc :: Parser Term
pSucc = do
  _ <- symbol "succ"
  n <- pConst <|> parens pTerm
  pure $ TmSucc n

pPred :: Parser Term
pPred = do
  _ <- symbol "pred"
  n <- pConst <|> parens pTerm
  pure $ TmPred n

pIsZero :: Parser Term
pIsZero = do
  _ <- symbol "isZero"
  n <- pConst <|> parens pTerm
  pure $ TmIsZero n

pZero :: Parser Term
pZero = do
  _ <- symbol "0" <|> symbol "zero"
  pure TmZero

pConst :: Parser Term
pConst = choice [pTrue, pFalse, pZero]

pTerm :: Parser Term
pTerm = choice [pIf, pSucc, pPred, pIsZero]

pSrc :: Parser Term
pSrc = between whitespace eof pTerm

parseCode :: String -> IO Term
parseCode src = case parse pSrc "" src of
  Left e -> putStrLn (errorBundlePretty e) >> exitFailure
  Right t -> return t
