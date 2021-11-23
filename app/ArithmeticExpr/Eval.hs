module Eval
  ( eval,
  )
where

import Data

eval1 :: Term -> Maybe Term
eval1 (TmIf p t1 t2) = case p of
  TmTrue -> Just t1
  TmFalse -> Just t2
  _ -> case eval1 p of
    Just p' -> Just (TmIf p' t1 t2)
    Nothing -> Nothing
eval1 (TmSucc n) = case eval1 n of
  Just n' -> Just (TmSucc n')
  Nothing -> Nothing
eval1 (TmPred n) = case n of
  TmZero -> Just TmZero
  TmSucc n1 -> if isNum n1 then Just n1 else Nothing
  _ -> case eval1 n of
    Just n' -> Just (TmPred n')
    Nothing -> Nothing
eval1 (TmIsZero n) = case n of
  TmZero -> Just TmTrue
  TmSucc n1 -> if isNum n1 then Just TmFalse else Nothing
  _ -> case eval1 n of
    Just n' -> Just (TmIsZero n')
    Nothing -> Nothing
eval1 _ = Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
