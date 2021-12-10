module Arith.Eval (eval) where

import Arith.Data (Term (..), isNum)

eval1 :: Term -> Maybe Term
eval1 (TmIf p t1 t2) = case p of
  TmTrue -> Just t1
  TmFalse -> Just t2
  _ -> (\p' -> Just (TmIf p' t1 t2)) =<< eval1 p
eval1 (TmSucc n) = (Just . TmSucc) =<< eval1 n
eval1 (TmPred n) = case n of
  TmZero -> Just TmZero
  TmSucc n1 -> if isNum n1 then Just n1 else Nothing
  _ -> (Just . TmPred) =<< eval1 n
eval1 (TmIsZero n) = case n of
  TmZero -> Just TmTrue
  TmSucc n1 -> if isNum n1 then Just TmFalse else Nothing
  _ -> (Just . TmIsZero) =<< eval1 n
eval1 _ = Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
