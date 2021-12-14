module STLC.Type (tyck) where

import Control.Applicative ((<**>))
import STLC.Syntax (Term (..), Type (..))

type TypeContext = [Type]

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
  TmPair t1 t2 -> TyPair <$> typeOf ctx t1 <*> typeOf ctx t2
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
  TmFix t -> case typeOf ctx t of
    Just (TyArr ty1 ty2) -> if ty1 == ty2 then Just ty1 else Nothing
    _ -> Nothing

tyck :: Term -> Term
tyck tm = case typeOf [] tm of
  Just _ -> tm
  Nothing -> error "failed to type check"
