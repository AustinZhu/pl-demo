module STLC.Type (tyck) where

import STLC.Syntax (Term (..), Type (..))

type TypeContext = [Type]

typeOf :: TypeContext -> Term -> Maybe Type
typeOf ctx tm = case tm of
  -- x:T ∈ Γ ⇒ Γ ⊢ x : T
  TmVar n -> if n < length ctx then Just (ctx !! n) else Nothing
  -- Γ,x:T₁ ⊢ t₂ : T₁→T₂ ⇒ Γ ⊢ λx:T₁.t₂ : T₁→T₂
  TmAbs _ ty t1 -> typeOf (ty : ctx) t1 >>= Just . TyArr ty
  -- Γ ⊢ t₁ : T₁₁→T₁₂ ∧ Γ ⊢ t₂ : T₁₁ ⇒ Γ ⊢ t₁ t₂ : T₁₂
  TmApp t1 t2 -> case typeOf ctx t1 of
    Just (TyArr ty1 ty2) -> case typeOf ctx t2 of
      Just ty -> if ty == ty1 then Just ty2 else Nothing
      Nothing -> Nothing
    _ -> Nothing
  -- ⊢ true : Bool
  TmTrue -> Just TyBool
  -- ⊢ false : Bool
  TmFalse -> Just TyBool

tyck :: Term -> Term
tyck tm = case typeOf [] tm of
  Just _ -> tm
  Nothing -> error "failed to type check"
