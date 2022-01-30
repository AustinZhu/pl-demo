module Subtyping.Type (tyck) where

import Control.Applicative ((<**>))
import Subtyping.Syntax (Term (..), Type (..))

type TypeContext = [Type]

(<:) :: Type -> Type -> Bool
(<:) ty1 TyTop = True
(<:) (TyArr t1 t2) (TyArr t1' t2') = (t1' <: t1) && (t2 <: t2')
(<:) (TyList t1) (TyList t2) = t1 <: t2

typeOf :: TypeContext -> Term -> Maybe Type
typeOf ctx tm = case tm of
  -- x:T ∈ Γ ⇒ Γ ⊢ x : T
  TmVar n -> if n < length ctx then Just (ctx !! n) else Nothing
  -- Γ,x:T₁ ⊢ t₂ : T₁→T₂ ⇒ Γ ⊢ λx:T₁.t₂ : T₁→T₂
  TmAbs _ ty t1 -> TyArr ty <$> typeOf (ty : ctx) t1
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
  -- ⊢ n : Nat
  TmInt _ -> Just TyNat
  -- ⊢ s : String
  TmString _ -> Just TyString
  -- ⊢ unit : Unit
  TmUnit -> Just TyUnit
  -- ⊢ succ : Nat → Nat
  TmSucc -> Just (TyArr TyNat TyNat)
  -- ⊢ pred : Nat → Nat
  TmPred -> Just (TyArr TyNat TyNat)
  -- ⊢ iszero : Nat → Bool
  TmIsZero -> Just (TyArr TyNat TyBool)
  TmLen -> Just (TyArr TyString TyNat)
  -- Γ ⊢ t₁ : T₁ ∧ Γ,t₁:T₁ ⊢ t₂ : T₂ ⇒ Γ ⊢ let x=t₁ in t₂ : T₂
  TmLet _ t1 t2 -> case typeOf ctx t1 of
    Just ty1 -> typeOf (ty1 : ctx) t2
    Nothing -> Nothing
  TmIf t1 t2 t3 -> case typeOf ctx t1 of
    Just TyBool -> let ty = typeOf ctx t2 in if ty == typeOf ctx t3 then ty else Nothing
    _ -> Nothing
  TmFix t -> case typeOf ctx t of
    Just (TyArr ty1 ty2) -> if ty1 == ty2 then Just ty1 else Nothing
    _ -> Nothing
  TmTuple ts -> case ts of
    [] -> Just TyUnit
    _ -> TyTuple <$> traverse (typeOf ctx) ts
  TmProjTuple t i -> case typeOf ctx t of
    Just (TyTuple ts) -> if i < length ts then Just (ts !! i) else Nothing
    _ -> Nothing
  TmRecord ts -> case ts of
    [] -> Just TyUnit
    _ -> TyRecord <$> traverse (\(n, t) -> (\ty -> (n, ty)) <$> typeOf ctx t) ts

tyck :: Term -> Term
tyck tm = case typeOf [] tm of
  Just _ -> tm
  Nothing -> error "failed to type check"
