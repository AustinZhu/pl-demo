module Arith.Eval (eval) where

import Arith.Syntax (Term (..))

isVal :: Term -> Bool
isVal t = case t of
  TmTrue -> True
  TmFalse -> True
  TmInt _ -> True
  _ -> False

eval1 :: Term -> Maybe Term
eval1 (TmIf p t1 t2) = case p of
  TmTrue -> Just t1
  TmFalse -> Just t2
  _ -> (\p' -> TmIf p' t1 t2) <$> eval1 p
eval1 (TmSucc t) = case t of
  TmInt n -> Just (TmInt (n + 1))
  _ -> TmSucc <$> eval1 t
eval1 (TmPred n) = case n of
  TmInt n1 -> if n1 == 0 then Just n else Just (TmInt (n1 - 1))
  _ -> TmPred <$> eval1 n
eval1 (TmIsZero n) = case n of
  TmInt n1 -> if n1 == 0 then Just TmTrue else Just TmFalse
  _ -> TmIsZero <$> eval1 n
eval1 _ = Nothing

eval :: Term -> Term
eval t =
  let res = maybe t eval (eval1 t)
   in if isVal res then res else error "evaluation stuck"
