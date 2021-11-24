module Lib where

import ArithmeticExpr.Eval (evalTerm)
import ArithmeticExpr.Parser (parseCode)

data Code = Code Lang String

data Lang = Arith | UTLC | STLC | Sub | SF | DTLC

evalCode :: Code -> String
evalCode (Code l c) = case l of
  Arith -> show $ evalTerm (parseCode c)
  UTLC -> error "not implemented"
  STLC -> error "not implemented"
  Sub -> error "not implemented"
  SF -> error "not implemented"
  DTLC -> error "not implemented"

help :: Lang -> String
help l = case l of
  Arith ->
    unlines
      [ "Syntax:",
        "  <term> ::= <bool> | if <term> then <term> else <term> | iszero <term> | succ <term> | pred <term> | 0",
        "  <bool> ::= true | false",
        "  <num>  ::= 0 | succ <num>"
      ]
  UTLC -> ""
  STLC -> ""
  Sub -> ""
  SF -> ""
  DTLC -> ""
