module Arith.Syntax where

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  | TmInt Int

instance Show Term where showsPrec = prettyTm

prettyTm :: Int -> Term -> ShowS
prettyTm prec = go (prec /= 0)
  where
    go :: Bool -> Term -> ShowS
    go p tm = case tm of
      TmTrue -> ("true" ++)
      TmFalse -> ("false" ++)
      TmIsZero n -> showParen p (("iszero " ++) . go True n)
      TmIf p1 t1 t2 -> showParen p (("if " ++) . go True p1 . (" then " ++) . go True t1 . (" else " ++) . go True t2)
      TmPred n -> showParen p (("pred " ++) . go True n)
      TmSucc n -> showParen p (("succ " ++) . go True n)
      TmInt i -> (show i ++)
