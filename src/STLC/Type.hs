module STLC.Type (tyck) where

import Control.Applicative ((<**>))
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
  -- ⊢ n : Nat
  TmInt _ -> Just TyNat
  -- ⊢ s : String
  TmString _ -> Just TyString
  -- ⊢ unit : Unit
  TmUnit -> Just TyUnit
  -- ⊢ succ : Nat → Nat
  TmSucc -> Just (TyArr TyNat TyNat)
  -- Γ ⊢ t₁ : T₁ ∧ Γ,t₁:T₁ ⊢ t₂ : T₂ ⇒ Γ ⊢ let x=t₁ in t₂ : T₂
  TmLet _ t1 t2 -> case typeOf ctx t1 of
    Just ty1 -> typeOf (ty1 : ctx) t2
    Nothing -> Nothing
  TmPair t1 t2 -> typeOf ctx t2 <**> (typeOf ctx t1 <**> Just TyPair)
  TmFst t -> case typeOf ctx t of
    Just (TyPair ty1 ty2) -> Just ty1
    _ -> Nothing
  TmSnd t -> case typeOf ctx t of
    Just (TyPair ty1 ty2) -> Just ty2
    _ -> Nothing
  TmInl t ty -> case ty of
    TyVariant tl _ -> if typeOf ctx t == Just tl then Just ty else Nothing
    _ -> Nothing
  TmInr t ty -> case ty of
    TyVariant _ tr -> if typeOf ctx t == Just tr then Just ty else Nothing
    _ -> Nothing
  TmCase t pl pr -> case typeOf ctx t of
    Just (TyVariant tyl tyr) ->
      let ty = typeOf (tyl : ctx) (snd pl)
       in if ty == typeOf (tyr : ctx) (snd pr)
            then ty
            else Nothing
    _ -> Nothing

tyck :: Term -> Term
tyck tm = case typeOf [] tm of
  Just _ -> tm
  Nothing -> error "failed to type check"
