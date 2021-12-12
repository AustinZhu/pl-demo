module STLC.Type (tyck) where

import STLC.Data (Term (..), Type (..))

type TypeContext = [Type]

typeOf :: TypeContext -> Term -> Either Term Type
typeOf ctx tm = case tm of
  TmVar n -> if n < length ctx then Right (ctx !! n) else Left tm
  TmAbs x ty t1 -> typeOf (ty : ctx) t1 >>= Right . TyArr ty
  TmApp t1 t2 -> case typeOf ctx t1 of
    Left tm -> Left tm
    Right (TyArr ty1 ty2) -> case typeOf ctx t2 of
      Left tm -> Left tm
      Right ty -> if ty == ty1 then Right ty2 else Left tm
    _ -> Left tm
  TmTrue -> Right TyBool
  TmFalse -> Right TyBool

tyck :: Term -> Term
tyck tm = case typeOf [] tm of
  Right _ -> tm
  Left term -> error ("Failed to type check " ++ show term)
