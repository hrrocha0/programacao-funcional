module Interpreter where

import AbsLF

import Prelude hiding (lookup)
import Tests

getName :: Function -> Ident
getName (Fun name _ _) = name

getParams :: Function -> [Ident]
getParams (Fun _ params _) = params

getExp :: Function -> Exp
getExp (Fun _ _ exp) = exp

{-
A função "executeP" passa a evaluar uma expressão, uma vez que não existem statements na LF1. 
Além disso, passa a retornar o resultado da avaliação e não mais o contexto,
pois a linguagem funcional não possui variáveis.
-}
executeP :: Program -> Valor
executeP (Prog fs) = eval (updatecF [] fs) (expMain fs)
  where
    expMain (f : xs)
      | getName f == Ident "main" = getExp f
      | otherwise = expMain xs

{-
A avaliação de EIf é implementada de maneira semelhante ao SIf da função "execute" na linguagem imperativa.
Enquanto na LI2 os termos "then" e "else" especificavam comandos a ser executados, na LF1 são expressions 
que são avaliadas.

A expressão ECall é avaliada procurando no contexto a função dada pelo seu identificador, e então a sua 
expressão é avaliada levando em consideração seus parâmetros e as funções definidas no contexto.
-}
eval :: RContext -> Exp -> Valor
eval context x = case x of
  ECon exp0 exp -> ValorStr (s (eval context exp0) ++ s (eval context exp))
  EAdd exp0 exp -> ValorInt (i (eval context exp0) + i (eval context exp))
  ESub exp0 exp -> ValorInt (i (eval context exp0) - i (eval context exp))
  EMul exp0 exp -> ValorInt (i (eval context exp0) * i (eval context exp))
  EDiv exp0 exp -> ValorInt (i (eval context exp0) `div` i (eval context exp))
  EOr exp0 exp -> ValorBool (b (eval context exp0) || b (eval context exp))
  EAnd exp0 exp -> ValorBool (b (eval context exp0) && b (eval context exp))
  ENot exp -> ValorBool (not (b (eval context exp)))
  EStr str -> ValorStr str
  ETrue -> ValorBool True
  EFalse -> ValorBool False
  EInt n -> ValorInt n
  EVar id -> lookup context id
  EIf exp expT expE ->
    if i (eval context exp) /= 0
      then eval context expT
      else eval context expE
  ECall id lexp -> eval (paramBindings ++ contextFunctions) (getExp funDef)
    where
      (ValorFun funDef) = lookup context id
      parameters = getParams funDef
      paramBindings = zip parameters (map (eval context) lexp)
      contextFunctions =
        filter
          ( \(i, v) -> case v of
              ValorFun _ -> True
              _ -> False
          )
          context

data Valor
  = ValorInt {i :: Integer}
  | ValorFun {f :: Function}
  | ValorStr {s :: String}
  | ValorBool {b :: Bool}

instance Show Valor where
  show :: Valor -> String
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun f) = show f

type RContext = [(Ident, Valor)]

lookup :: RContext -> Ident -> Valor
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> Ident -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv

updatecF :: RContext -> [Function] -> RContext
updatecF = foldl (\ c f -> update c (getName f) (ValorFun f))
