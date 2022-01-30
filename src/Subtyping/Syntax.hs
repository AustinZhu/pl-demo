module Subtyping.Syntax where

import Data.Map (Map)
import qualified Data.Map as M

data Term
  = TmVar Int
  | TmAbs String Type Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmUnit
  | TmInt Int
  | TmString String
  | TmLen
  | TmSucc
  | TmIsZero
  | TmPred
  | TmLet String Term Term
  | TmIf Term Term Term
  | TmTuple [Term]
  | TmProjTuple Term Int
  | TmRecord [(String, Term)]
  | TmProjRecord Term String
  | TmInj String Term Type
  | TmCase Term [(String, Pattern)]
  | TmFix Term

type Pattern = (String, Term)

data Type
  = TyBool
  | TyNat
  | TyString
  | TyUnit
  | TyTuple [Type]
  | TyRecord [(String, Type)]
  | TyVariant Type Type
  | TyArr Type Type
  | TyList Type
  | TyTop
  deriving (Eq)

type NameContext = [String]

fresh :: NameContext -> String -> (NameContext, String)
fresh ctx x = if isNameBound ctx x then fresh ctx (x ++ "'") else (x : ctx, x)
  where
    isNameBound ctx' x' = case ctx' of
      [] -> False
      (y : ys) -> y == x' || isNameBound ys x

indexOf :: NameContext -> String -> Int
indexOf [] x = error (concat ["indentifier ", x, " is not in scope"])
indexOf (y : ys) x = if y == x then 0 else 1 + indexOf ys x

instance Show Term where showsPrec = prettyTm

prettyTm :: Int -> Term -> ShowS
prettyTm prec = go (prec /= 0) []
  where
    go :: Bool -> NameContext -> Term -> ShowS
    go p ctx tm = case tm of
      TmAbs x _ t1 ->
        let (ctx', x') = fresh ctx x
         in showParen p ((concat ["λ", x', ". "] ++) . go False ctx' t1)
      TmApp t1 t2 -> showParen p $ go True ctx t1 . (" " ++) . go True ctx t2
      TmVar x -> (ctx !! x ++)
      TmTrue -> ("true" ++)
      TmFalse -> ("false" ++)
      TmUnit -> ("unit" ++)
      TmInt x -> (show x ++)
      TmString x -> (show x ++)
      TmSucc -> ("succ" ++)
      TmIsZero -> ("iszero" ++)
      TmPred -> ("pred" ++)
      TmLet x t1 t2 ->
        let (ctx', x') = fresh ctx x
         in showParen p ((concat ["let ", x', " = "] ++) . go False ctx' t1 . (" in " ++) . go False ctx' t2)
      TmIf t1 t2 t3 ->
        showParen p $ ("if " ++) . go True ctx t1 . (" then " ++) . go True ctx t2 . (" else " ++) . go True ctx t3
      TmTuple ts ->
        ("{" ++)
          . foldr1 (\acc x -> ("," ++) . acc) (map (go False ctx) ts)
          . ("}" ++)
      TmProjTuple t i -> go True ctx t . ("." ++) . (show i ++)
      TmRecord m ->
        ("{" ++)
          . foldr1 (\acc x -> ("," ++) . acc) (map (\(k, v) -> (k ++) . ("=" ++) . go False ctx v) m)
          . ("}" ++)
      TmProjRecord t s -> go True ctx t . ("." ++) . (s ++)
      TmInj s t _ -> showParen p $ (s ++) . (" " ++) . go True ctx t
      TmFix t1 -> showParen p $ ("fix " ++) . go True ctx t1
      TmLen -> showParen p ("len " ++)
      TmCase t m ->
        showParen p $
          ("case " ++) . go True ctx t . (" of\n    " ++)
            . foldr1 (\acc x -> acc . ("\n  | " ++)) (map (\(k, v) -> (k ++) . (" " ++) . (fst v ++) . (" => " ++) . go False ctx (snd v)) m)
