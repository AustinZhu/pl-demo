module UTLC.Data where

data Term
  = TmVar Int
  | TmAbs String Term
  | TmApp Term Term

type Context = [String]

data Closure = Closure {ctx :: Context, tm :: Term}

fresh :: Context -> String -> (Context, String)
fresh ctx x = if isNameBound ctx x then fresh ctx (x ++ "'") else (x : ctx, x)
  where
    isNameBound ctx' x' = case ctx' of
      [] -> False
      (y : ys) -> y == x' || isNameBound ys x

idx2Name :: Context -> Int -> String
idx2Name ctx i = ctx !! i

name2Idx :: Context -> String -> Int
name2Idx [] x = error (concat ["Indentifier ", x, " is not in scope."])
name2Idx (y : ys) x = if y == x then 0 else 1 + name2Idx ys x

instance Show Term where showsPrec = prettyTm

prettyTm :: Int -> Term -> ShowS
prettyTm prec = go (prec /= 0) []
  where
    go :: Bool -> Context -> Term -> ShowS
    go p ctx tm = case tm of
      TmAbs x t1 ->
        let (ctx', x') = fresh ctx x
         in showParen p ((concat ["λ", x', ". "] ++) . go False ctx' t1)
      TmApp t1 t2 -> go True ctx t1 . (" " ++) . go True ctx t2
      TmVar x -> (idx2Name ctx x ++)
