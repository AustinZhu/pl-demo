module STLC.Data where

data Term
  = TmVar Int
  | TmAbs String Type Term
  | TmApp Term Term
  | TmTrue
  | TmFalse

data Type = TyBool | TyArr Type Type deriving (Eq)

type NameContext = [String]

data Closure = Closure {ctx :: NameContext, tm :: Term}

fresh :: NameContext -> String -> (NameContext, String)
fresh ctx x = if isNameBound ctx x then fresh ctx (x ++ "'") else (x : ctx, x)
  where
    isNameBound ctx' x' = case ctx' of
      [] -> False
      (y : ys) -> y == x' || isNameBound ys x

indexOf :: NameContext -> String -> Int
indexOf [] x = error (concat ["Indentifier ", x, " is not in scope."])
indexOf (y : ys) x = if y == x then 0 else 1 + indexOf ys x

instance Show Term where showsPrec = prettyTm

prettyTm :: Int -> Term -> ShowS
prettyTm prec = go (prec /= 0) []
  where
    go :: Bool -> NameContext -> Term -> ShowS
    go p ctx tm = case tm of
      TmAbs x _ t1 ->
        let (ctx', x') = fresh ctx x
         in showParen p ((concat ["λ", x', ". "] ++) . go False ctx' t1)
      TmApp t1 t2 -> go True ctx t1 . (" " ++) . go True ctx t2
      TmVar x -> (ctx !! x ++)
      TmTrue -> ("true" ++)
      TmFalse -> ("false" ++)
