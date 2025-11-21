module InterpreterOld where

import AbsLF
import AbsLFAux

import Prelude hiding (lookup)

data Valor
  = ValorInt {i :: Integer}
  | ValorBool {b :: Bool}
  | ValorStr {s :: String}
  | ValorFun {f :: Function}
  deriving (Eq)

instance Show Valor where
  show :: Valor -> String
  show (ValorInt i) = show i
  show (ValorBool b) = show b
  show (ValorStr s) = s
  show (ValorFun f) = show f

type RContext = [(Ident, Valor)]

executeP :: Program -> Valor
executeP (Prog fs) = eval (updatecF [] fs) (expMain fs)
  where
    expMain :: [Function] -> Exp
    expMain (f : fs')
      | getName f == Ident "main" = getExp f
      | otherwise = expMain fs'

eval :: RContext -> Exp -> Valor
eval ctx (EIf expC expT expE) =
  let v1 = i $ eval ctx expC
   in if v1 /= 0
        then eval ctx expT
        else eval ctx expE
eval ctx (EOr exp0 exp) =
  let v1 = b $ eval ctx exp0
      v2 = b $ eval ctx exp
   in ValorBool (v1 || v2)
eval ctx (EAnd exp0 exp) =
  let v1 = b $ eval ctx exp0
      v2 = b $ eval ctx exp
   in ValorBool (v1 && v2)
eval ctx (ENot exp) =
  let v1 = b $ eval ctx exp
   in ValorBool (not v1)
eval ctx (ECon exp0 exp) =
  let v1 = s $ eval ctx exp0
      v2 = s $ eval ctx exp
   in ValorStr (v1 ++ v2)
eval ctx (EAdd exp0 exp) =
  let v1 = i $ eval ctx exp0
      v2 = i $ eval ctx exp
   in ValorInt (v1 + v2)
eval ctx (ESub exp0 exp) =
  let v1 = i $ eval ctx exp0
      v2 = i $ eval ctx exp
   in ValorInt (v1 - v2)
eval ctx (EMul exp0 exp) =
  let v1 = i $ eval ctx exp0
      v2 = i $ eval ctx exp
   in ValorInt (v1 * v2)
eval ctx (EDiv exp0 exp) =
  let v1 = i $ eval ctx exp0
      v2 = i $ eval ctx exp
   in ValorInt (v1 `div` v2)
eval ctx (ECall id lexp) =
  let (ValorFun f) = lookup ctx id
      parameterBindings = zip (getParams f) (map (eval ctx) lexp)
      contextFunctions = filter isFunction ctx
   in eval (parameterBindings ++ contextFunctions) (getExp f)
  where
    isFunction :: (Ident, Valor) -> Bool
    isFunction (_, ValorFun _) = True
    isFunction _ = False
eval ctx (EVar id) = lookup ctx id
eval _ (EInt n) = ValorInt n
eval _ (EStr s) = ValorStr s
eval _ ETrue = ValorBool True
eval _ EFalse = ValorBool False

lookup :: RContext -> Ident -> Valor
lookup ((id, v) : cs) id'
  | id == id' = v
  | otherwise = lookup cs id'

update :: RContext -> Ident -> Valor -> RContext
update [] id' v' = [(id', v')]
update ((id, v) : cs) id' v'
  | id == id' = (id', v') : cs
  | otherwise = (id, v) : update cs id' v'

updatecF :: RContext -> [Function] -> RContext
updatecF = foldl (\ctx f -> update ctx (getName f) (ValorFun f))
