module UTLC.Syntax where

data Term
  = TmVar Int
  | TmAbs String Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmInt Int
  | TmIf Term Term Term
  | TmIsZero Term
  | TmSucc Term
  | TmPred Term
  | TmLet String Term Term
  | TmFix Term

type Context = [String]

fresh :: Context -> String -> (Context, String)
fresh ctx x = if isNameBound ctx x then fresh ctx (x ++ "'") else (x : ctx, x)
  where
    isNameBound ctx' x' = case ctx' of
      [] -> False
      (y : ys) -> y == x' || isNameBound ys x

indexOf :: Context -> String -> Int
indexOf [] x = error (concat ["indentifier ", x, " is not in scope"])
indexOf (y : ys) x = if y == x then 0 else 1 + indexOf ys x

instance Show Term where showsPrec = prettyTm

prettyTm :: Int -> Term -> ShowS
prettyTm prec = go (prec /= 0) []
  where
    go :: Bool -> Context -> Term -> ShowS
    go p ctx tm = case tm of
      TmAbs x t1 ->
        let (ctx', x') = fresh ctx x
         in showParen p ((concat ["λ", x', ". "] ++) . go False ctx' t1)
      TmApp t1 t2 -> showParen p $ go True ctx t1 . (" " ++) . go True ctx t2
      TmVar x -> (ctx !! x ++)
      TmTrue -> ("true" ++)
      TmFalse -> ("false" ++)
      TmIsZero n -> showParen p (("iszero " ++) . go True ctx n)
      TmIf p1 t1 t2 -> showParen p (("if " ++) . go True ctx p1 . (" then " ++) . go True ctx t1 . (" else " ++) . go True ctx t2)
      TmPred n -> showParen p (("pred " ++) . go True ctx n)
      TmSucc n -> showParen p (("succ " ++) . go True ctx n)
      TmInt i -> (show i ++)
      TmLet x t1 t2 -> showParen p ((concat ["let ", x, " = "] ++) . go True ctx t1 . (" in " ++) . go False ctx t2)
      TmFix t1 -> showParen p (("fix " ++) . go True ctx t1)
