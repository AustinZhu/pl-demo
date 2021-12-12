module Lib where

import Arith.Eval (eval)
import Arith.Parser (parseCode)
import UTLC.Eval (eval)
import UTLC.Parser (parseCode)
import STLC.Eval (eval)
import STLC.Parser (parseCode)

data Code = Code Lang String

data Lang = Arith | UTLC | STLC | Sub | SF | DTLC

mapLang :: String -> Maybe Lang
mapLang s = case s of
  "arith" -> Just Arith
  "utlc" -> Just UTLC
  "stlc" -> Just STLC
  "sub" -> Just Sub
  "sf" -> Just SF
  "dtlc" -> Just DTLC
  _ -> Nothing

evalCode :: Code -> String
evalCode (Code l c) = case l of
  Arith -> show $ Arith.Eval.eval (Arith.Parser.parseCode c)
  UTLC -> show $ UTLC.Eval.eval (UTLC.Parser.parseCode c)
  STLC -> show $ STLC.Eval.eval (STLC.Parser.parseCode c)
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
  UTLC ->
    unlines
      [ "Syntax:",
        "  <term> ::= x | \\x.<term> | <term> <term>"
      ]
  STLC -> 
    unlines
      [ "Syntax:",
        "  <term> ::= x | \\x:<type>.<term> | <term> <term>",
        "  <type> ::= Bool | <type> -> <type>"
      ]
  Sub -> ""
  SF -> ""
  DTLC -> ""
