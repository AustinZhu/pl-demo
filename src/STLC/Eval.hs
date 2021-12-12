module STLC.Eval (eval) where

import STLC.Syntax (NameContext, Term (..))
import STLC.Type (tyck)

-- | Shift the de Bruijn indices of a term by the given amount.
shift :: Int -> Term -> Term
shift i t = walk 0 t
  where
    walk c t = case t of
      TmVar x -> if x >= c then TmVar (x + i) else TmVar x
      TmAbs x ty t1 -> TmAbs x ty (walk (c + 1) t1)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
      _ -> t

-- | Substitute term s for the variables of index j in term t.
subst :: Int -> Term -> Term -> Term
subst j s t = walk 0 t
  where
    walk c t = case t of
      TmVar x -> if x == j + c then shift c s else TmVar x
      TmAbs x ty t1 -> TmAbs x ty (walk (c + 1) t1)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
      _ -> t

isVal :: Term -> Bool
isVal t = case t of
  TmAbs {} -> True
  TmTrue -> True
  TmFalse -> True
  TmUnit -> True
  TmString _ -> True
  TmInt _ -> True
  _ -> False

-- | Beta reduction
substTm :: Term -> Term -> Term
substTm s t = shift (-1) (subst 0 (shift 1 s) t)

eval1 :: Term -> Maybe Term
eval1 t = case t of
  TmApp lam@(TmAbs x _ t1) t2 ->
    if isVal t2
      then Just (substTm t2 t1)
      else eval1 t2 >>= (Just . TmApp lam)
  TmApp TmSucc t1 -> case t1 of
    TmInt n -> Just (TmInt (n + 1))
    _ -> eval1 t1 >>= (Just . TmApp TmSucc)
  TmApp t1 t2 -> eval1 t1 >>= (Just . (`TmApp` t2))
  TmLet x t1 t2 ->
    if isVal t1
      then Just (substTm t1 t2)
      else eval1 t1 >>= Just . (\t1' -> TmLet x t1' t2)
  _ -> Nothing

eval' :: Term -> Term
eval' t = maybe t eval' (eval1 t)

eval :: Term -> Term
eval t = eval' (tyck t)
