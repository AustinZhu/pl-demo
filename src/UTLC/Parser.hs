module UTLC.Parser (parseCode) where

import Data.Functor (($>))
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
    (<|>), many
  )
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import UTLC.Syntax (Context, Term (..), indexOf)

type Parser = Parsec Void String

whitespace :: Parser ()
whitespace = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

symbol :: String -> Parser String
symbol = L.symbol whitespace

keywords :: [String]
keywords = ["let", "in", "if", "then", "else", "true", "false", "succ", "fix", "pred", "iszero"]

variable :: Parser String
variable = do
  first <- C.letterChar
  rest <- many (C.alphaNumChar <|> C.char '_')
  let name = (first : rest)
   in if name `elem` keywords
        then fail (show name ++ " is a keyword")
        else pure (first : rest)

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

pIf :: Context -> Parser Term
pIf ctx = do
  symbol "if"
  p <- pAtom ctx
  symbol "then"
  t1 <- pAtom ctx
  symbol "else"
  t2 <- pAtom ctx
  pure $ TmIf p t1 t2

pSucc :: Context -> Parser Term
pSucc ctx = do
  symbol "succ"
  TmSucc <$> pAtom ctx

pPred :: Context -> Parser Term
pPred ctx = do
  symbol "pred"
  TmPred <$> pAtom ctx

pIsZero :: Context -> Parser Term
pIsZero ctx = do
  symbol "iszero"
  TmIsZero <$> pAtom ctx

pLet :: Context -> Parser Term
pLet ctx = do
  symbol "let"
  x <- lexeme variable
  symbol "="
  tm1 <- pTerm ctx
  symbol "in"
  tm2 <- pAtom (x : ctx)
  pure $ TmLet x tm1 tm2

pFix :: Context -> Parser Term
pFix ctx = do
  symbol "fix"
  t <- pAtom ctx
  pure $ TmFix t

pVar :: Context -> Parser Term
pVar ctx = do
  x <- lexeme (some C.letterChar)
  let idx = indexOf ctx x
  pure $ TmVar idx

pLam :: Context -> Parser Term
pLam ctx = do
  symbol "\\"
  x <- lexeme (some (C.letterChar <|> C.char '_'))
  symbol "."
  t <- pTerm (x : ctx)
  pure $ TmAbs x t

pApp :: Context -> Parser Term
pApp ctx = do
  t1 <- pAtom ctx
  C.space
  t2 <- pAtom ctx
  pure $ TmApp t1 t2

pTerm :: Context -> Parser Term
pTerm ctx = pLam ctx <|> pConst <|> pIf ctx <|> pSucc ctx <|> pPred ctx <|> pIsZero ctx <|> try (pApp ctx) <|> pVar ctx

pAtom :: Context -> Parser Term
pAtom ctx = parens (pTerm ctx) <|> pConst <|> pVar ctx

pSrc :: Parser Term
pSrc = between whitespace eof (pTerm [])

parseCode :: String -> Term
parseCode src = case parse pSrc "" src of
  Left e -> error (errorBundlePretty e)
  Right t -> t
