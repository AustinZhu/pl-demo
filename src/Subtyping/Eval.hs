module Subtyping.Eval where

import Subtyping.Syntax (NameContext, Term (..))
import Subtyping.Type (tyck)

-- | Shift the de Bruijn indices of a term by the given amount.
shift :: Int -> Term -> Term
shift i t = walk 0 t
  where
    walk c t = case t of
      TmVar x -> if x >= c then TmVar (x + i) else TmVar x
      TmAbs x ty t1 -> TmAbs x ty (walk (c + 1) t1)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
      TmLet x t1 t2 -> TmLet x (walk c t1) (walk (c + 1) t2)
      TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
      TmFix t1 -> TmFix (walk c t1)

      _ -> t

-- | Substitute term s for the variables of index j in term t.
subst :: Int -> Term -> Term -> Term
subst j s t = walk 0 t
  where
    walk c t = case t of
      TmVar x -> if x == j + c then shift c s else TmVar x
      TmAbs x ty t1 -> TmAbs x ty (walk (c + 1) t1)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
      TmLet x t1 t2 -> TmLet x (walk c t1) (walk (c + 1) t2)
      TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
      TmFix t1 -> TmFix (walk c t1)
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
eval1 (TmApp t1 t2) = case t1 of
  TmAbs x _ t ->
    if isVal t2
      then Just (substTm t2 t)
      else TmApp t1 <$> eval1 t2
  TmSucc -> case t2 of
    TmInt n -> Just (TmInt (n + 1))
    _ -> TmApp TmSucc <$> eval1 t2
  TmPred -> case t2 of
    TmInt n -> if n == 0 then Just t2 else Just (TmInt (n - 1))
    _ -> TmApp TmPred <$> eval1 t2
  TmIsZero -> case t2 of
    TmInt 0 -> Just TmTrue
    TmInt _ -> Just TmFalse
    _ -> TmApp TmIsZero <$> eval1 t2
  TmLen -> case t2 of
    TmString s -> Just (TmInt (length s))
    _ -> TmApp TmLen <$> eval1 t2
  _ -> (`TmApp` t2) <$> eval1 t1
eval1 (TmLet x t1 t2) =
  if isVal t1
    then Just (substTm t1 t2)
    else (\t1' -> TmLet x t1' t2) <$> eval1 t1
eval1 (TmIf b t1 t2) = case b of
  TmTrue -> Just t1
  TmFalse -> Just t2
  _ -> (\b' -> TmIf b' t1 t2) <$> eval1 b
eval1 (TmFix t) = case t of
  TmAbs _ _ t1 -> Just (substTm (TmFix t) t1)
  _ -> TmFix <$> eval1 t
eval1 t = Nothing

eval :: Term -> Term
eval t =
  let res = eval' (tyck t)
   in if isVal res then res else error "evaluation stuck"
  where
    eval' :: Term -> Term
    eval' t = maybe t eval' (eval1 t)
