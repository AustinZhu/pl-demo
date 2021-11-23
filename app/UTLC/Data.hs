module Expr where

data Term
  = TmVar Int Int
  | TmAbs String Term
  | TmApp Term Term

instance Show Term where showsPrec = prettyTm

fresh :: [String] -> String -> ([String], String)
fresh ctx x = if isNameBound ctx x then fresh ctx (x ++ "'") else (x:ctx, x) where
  isNameBound ctx' x' = case ctx' of
    [] -> False
    y:ys -> if y == x' then True else isNameBound ys x

idx2Name :: [String] -> Int -> String
idx2Name ctx i = ctx !! i

prettyTm :: Int -> Term -> ShowS
prettyTm prec = go (prec /= 0) []
  where
    go :: Bool -> [String] -> Term -> ShowS
    go p ctx tm = case tm of
      TmAbs x t1 -> let (ctx', x') = fresh ctx x in
        showParen p (("λ" ++) . (x' ++) . (". " ++ ) . go False ctx' t1)
      TmApp t1 t2 -> go True ctx t1 . (" " ++) . go True ctx t2
      TmVar x n -> if ctxlen ctx == n then (idx2name ctx x ++) else const "bad index"