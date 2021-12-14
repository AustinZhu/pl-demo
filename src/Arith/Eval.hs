module Arith.Eval (eval) where

import Arith.Data (Term (..), isNum)

eval1 :: Term -> Maybe Term
eval1 (TmIf p t1 t2) = case p of
  TmTrue -> Just t1
  TmFalse -> Just t2
  _ -> (\p' -> TmIf p' t1 t2) <$> eval1 p
eval1 (TmSucc (TmInt n)) = Just (TmInt (n + 1))
eval1 (TmSucc n) = TmSucc <$> eval1 n
eval1 (TmPred n) = case n of
  TmInt n1 -> Just (TmInt (n1 - 1))
  TmSucc n1 -> if isNum n1 then Just n1 else Nothing
  _ -> TmPred <$> eval1 n
eval1 (TmIsZero n) = case n of
  TmInt n1 -> if n1 == 0 then Just TmTrue else Just TmFalse
  TmSucc n1 -> if isNum n1 then Just TmFalse else Nothing
  _ -> TmIsZero <$> eval1 n
eval1 _ = Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
