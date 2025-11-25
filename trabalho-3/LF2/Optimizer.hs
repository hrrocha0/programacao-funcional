module Optimizer where

import AbsLF
import ErrM

import Interpreter

import Data.Generics

optimizeP :: Program -> Program
optimizeP = everywhere (mkT optimizeE)
  where
    optimizeE :: Exp -> Exp
    optimizeE exp@(EIf expC expT expE) =
      case expC of
        EInt n ->
          if n /= 0
            then expT
            else expE
        _ -> exp
    optimizeE exp =
      let (Ok v) = fst <$> eval ([], []) exp
       in if isGroundE exp
            then wrapValueExpression v
            else exp

isGroundE :: Exp -> Bool
isGroundE = everything (&&) (True `mkQ` isGround)
  where
    isGround :: Exp -> Bool
    isGround (EVar _) = False
    isGround (ECall _ _) = False
    isGround _ = True

wrapValueExpression :: Valor -> Exp
wrapValueExpression (ValorInt i) = EInt i
wrapValueExpression (ValorStr s) = EStr s
wrapValueExpression (ValorBool True) = ETrue
wrapValueExpression (ValorBool False) = EFalse
