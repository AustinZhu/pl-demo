module UTLC.Eval (eval) where

import UTLC.Syntax (Context, Term (..))

-- | Shift the de Bruijn indices of a term by the given amount.
shift :: Int -> Term -> Term
shift i t = walk 0 t
  where
    walk c t = case t of
      TmVar x -> if x >= c then TmVar (x + i) else TmVar x
      TmAbs x t1 -> TmAbs x (walk (c + 1) t1)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
      TmSucc t1 -> TmSucc (walk c t1)
      TmPred t1 -> TmPred (walk c t1)
      TmIsZero t1 -> TmIsZero (walk c t1)
      TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
      TmLet x t1 t2 -> TmLet x (walk c t1) (walk (c + 1) t2)
      _ -> t

-- | Substitute term s for the variables of index j in term t.
subst :: Int -> Term -> Term -> Term
subst j s t = walk 0 t
  where
    walk c t = case t of
      TmVar x -> if x == j + c then shift c s else TmVar x
      TmAbs x t1 -> TmAbs x (walk (c + 1) t1)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
      TmSucc t1 -> TmSucc (walk c t1)
      TmPred t1 -> TmPred (walk c t1)
      TmIsZero t1 -> TmIsZero (walk c t1)
      TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
      _ -> t

isVal :: Term -> Bool
isVal t = case t of
  TmAbs _ _ -> True
  TmInt _ -> True
  TmTrue -> True
  TmFalse -> True
  _ -> False

-- | Beta reduction
substTm :: Term -> Term -> Term
substTm s t = shift (-1) (subst 0 (shift 1 s) t)

eval1 :: Term -> Maybe Term
eval1 (TmApp t1 t2) = case t1 of
  TmAbs _ t ->
    if isVal t2
      then Just (substTm t2 t)
      else TmApp t1 <$> eval1 t2
  _ -> (`TmApp` t2) <$> eval1 t1
eval1 (TmIf p t1 t2) = case p of
  TmTrue -> Just t1
  TmFalse -> Just t2
  _ -> (\p' -> TmIf p' t1 t2) <$> eval1 p
eval1 (TmSucc n) = case n of
  TmInt n1 -> Just (TmInt (n1 + 1))
  _ -> TmPred <$> eval1 n
eval1 (TmPred n) = case n of
  TmInt n1 -> if n1 == 0 then Just n else Just (TmInt (n1 - 1))
  _ -> TmPred <$> eval1 n
eval1 (TmIsZero n) = case n of
  TmInt n1 -> if n1 == 0 then Just TmTrue else Just TmFalse
  _ -> TmIsZero <$> eval1 n
eval1 (TmFix t) = case t of
  TmAbs _ t1 -> Just (substTm (TmFix t) t1)
  _ -> TmFix <$> eval1 t
eval1 _ = Nothing

eval :: Term -> Term
eval t =
  let res = maybe t eval (eval1 t)
   in if isVal res then res else error "evaluation stuck"
