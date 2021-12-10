module UTLC.Eval (eval) where

import UTLC.Data (Closure (..), Context, Term (..))

-- | Shift the de Bruijn indices of a term by the given amount.
shift :: Int -> Term -> Term
shift i t = walk 0 t
  where
    walk c t = case t of
      TmVar x -> if x >= c then TmVar (x + i) else TmVar x
      TmAbs x t1 -> TmAbs x (walk (c + 1) t1)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

-- | Substitute term s for the variables of index j in term t.
subst :: Int -> Term -> Term -> Term
subst j s t = walk 0 t
  where
    walk c t = case t of
      TmVar x -> if x == j + c then shift c s else TmVar x
      TmAbs x t1 -> TmAbs x (walk (c + 1) t1)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

isVal :: Context -> Term -> Bool
isVal ctx t = case t of
  TmAbs _ _ -> True
  _ -> False

-- | Beta reduction
substTm :: Term -> Term -> Term
substTm s t = shift (-1) (subst 0 (shift 1 s) t)

eval1 :: Closure -> Maybe Term
eval1 (Closure ctx t) = case t of
  TmApp lam@(TmAbs x t1) t2 ->
    if isVal ctx t2
      then Just (substTm t2 t1)
      else (Just . TmApp lam) =<< eval1 (Closure ctx t2)
  TmApp t1 t2 -> (\s -> Just (TmApp s t2)) =<< eval1 (Closure ctx t1)
  _ -> Nothing

eval :: Closure -> Closure
eval cls@(Closure ctx t) = maybe cls (eval . Closure ctx) (eval1 cls)
