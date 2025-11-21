module Interpreter where

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

data FunCall = FunCall Ident [Valor] deriving (Eq, Show)

type RContext = [(Ident, Valor)]

type FContext = [(FunCall, Valor)]

type Context = (RContext, FContext)

executeP :: Program -> Valor
executeP (Prog fs) = fst $ eval (updatecF [] fs, []) (expMain fs)
  where
    expMain :: [Function] -> Exp
    expMain (f : fs')
      | getName f == Ident "main" = getExp f
      | otherwise = expMain fs'

eval :: Context -> Exp -> (Valor, FContext)
eval (rctx, fctx) (EIf expC expT expE) =
  let (v1, fctx1) = eval (rctx, fctx) expC
   in if i v1 /= 0
        then eval (rctx, fctx1) expT
        else eval (rctx, fctx1) expE
eval (rctx, fctx) (EOr exp0 exp) =
  let (v1, fctx1) = eval (rctx, fctx) exp0
      (v2, fctx2) = eval (rctx, fctx1) exp
   in (ValorBool (b v1 || b v2), fctx2)
eval (rctx, fctx) (EAnd exp0 exp) =
  let (v1, fctx1) = eval (rctx, fctx) exp0
      (v2, fctx2) = eval (rctx, fctx1) exp
   in (ValorBool (b v1 && b v2), fctx2)
eval (rctx, fctx) (ENot exp) =
  let (v1, fctx1) = eval (rctx, fctx) exp
   in (ValorBool (not $ b v1), fctx1)
eval (rctx, fctx) (ECon exp0 exp) =
  let (v1, fctx1) = eval (rctx, fctx) exp0
      (v2, fctx2) = eval (rctx, fctx1) exp
   in (ValorStr (s v1 ++ s v2), fctx2)
eval (rctx, fctx) (EAdd exp0 exp) =
  let (v1, fctx1) = eval (rctx, fctx) exp0
      (v2, fctx2) = eval (rctx, fctx1) exp
   in (ValorInt (i v1 + i v2), fctx2)
eval (rctx, fctx) (ESub exp0 exp) =
  let (v1, fctx1) = eval (rctx, fctx) exp0
      (v2, fctx2) = eval (rctx, fctx1) exp
   in (ValorInt (i v1 - i v2), fctx2)
eval (rctx, fctx) (EMul exp0 exp) =
  let (v1, fctx1) = eval (rctx, fctx) exp0
      (v2, fctx2) = eval (rctx, fctx1) exp
   in (ValorInt (i v1 * i v2), fctx2)
eval (rctx, fctx) (EDiv exp0 exp) =
  let (v1, fctx1) = eval (rctx, fctx) exp0
      (v2, fctx2) = eval (rctx, fctx1) exp
   in (ValorInt (i v1 `div` i v2), fctx2)
eval (rctx, fctx) (ECall id lexp) =
  let (ValorFun funDef) = lookup rctx id
      (args, fctx') = evalArgs (rctx, fctx) lexp
      funCall = FunCall id args
      parameterBindings = zip (getParams funDef) args
      contextFunctions = filter isFunction rctx
   in case lookupMemo fctx' funCall of
        Just rv -> (rv, fctx')
        Nothing -> memoize funCall $ eval (parameterBindings ++ contextFunctions, fctx') (getExp funDef)
  where
    evalArgs :: Context -> [Exp] -> ([Valor], FContext)
    evalArgs (_, fctx) [] = ([], fctx)
    evalArgs (rctx, fctx) (e : es) =
      let (v, fctx1) = eval (rctx, fctx) e
          (vs, fctx2) = evalArgs (rctx, fctx1) es
       in (v : vs, fctx2)

    isFunction :: (Ident, Valor) -> Bool
    isFunction (_, ValorFun _) = True
    isFunction _ = False

    memoize :: FunCall -> (Valor, FContext) -> (Valor, FContext)
    memoize fc (v, fctx) = (v, updateMemo fctx fc v)
eval (rctx, fctx) (EVar id) = (lookup rctx id, fctx)
eval (_, fctx) (EInt n) = (ValorInt n, fctx)
eval (_, fctx) (EStr s) = (ValorStr s, fctx)
eval (_, fctx) ETrue = (ValorBool True, fctx)
eval (_, fctx) EFalse = (ValorBool False, fctx)

lookup :: RContext -> Ident -> Valor
lookup ((id, v) : cs) id'
  | id == id' = v
  | otherwise = lookup cs id'

lookupMemo :: FContext -> FunCall -> Maybe Valor
lookupMemo [] _ = Nothing
lookupMemo ((fc, v) : cs) fc'
  | fc == fc' = Just v
  | otherwise = lookupMemo cs fc'

update :: RContext -> Ident -> Valor -> RContext
update [] id' v' = [(id', v')]
update ((id, v) : cs) id' v'
  | id == id' = (id', v') : cs
  | otherwise = (id, v) : update cs id' v'

updateMemo :: FContext -> FunCall -> Valor -> FContext
updateMemo [] fc' v' = [(fc', v')]
updateMemo ((fc, v) : cs) fc' v'
  | fc == fc' = (fc', v') : cs
  | otherwise = (fc, v) : updateMemo cs fc' v'

updatecF :: RContext -> [Function] -> RContext
updatecF = foldl (\ctx f -> update ctx (getName f) (ValorFun f))
