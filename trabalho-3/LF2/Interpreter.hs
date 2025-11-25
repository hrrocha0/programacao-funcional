module Interpreter where

import AbsLF
import AbsLFAux
import PrintLF
import ErrM

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

executeP :: Program -> Err Valor
executeP (Prog fs) = fst <$> eval (updatecF [] fs, []) (expMain fs)
  where
    expMain :: [Function] -> Exp
    expMain (f : fs')
      | getName f == Ident "main" = getExp f
      | otherwise = expMain fs'

eval :: Context -> Exp -> Err (Valor, FContext)
eval (rctx, fctx) (EIf expC expT expE) = do
  (v1, fctx1) <- eval (rctx, fctx) expC

  if i v1 /= 0
    then eval (rctx, fctx1) expT
    else eval (rctx, fctx1) expE
eval (rctx, fctx) (EOr exp0 exp) = do
  (v1, fctx1) <- eval (rctx, fctx) exp0
  (v2, fctx2) <- eval (rctx, fctx1) exp
  return (ValorBool (b v1 || b v2), fctx2)
eval (rctx, fctx) (EAnd exp0 exp) = do
  (v1, fctx1) <- eval (rctx, fctx) exp0
  (v2, fctx2) <- eval (rctx, fctx1) exp
  return (ValorBool (b v1 && b v2), fctx2)
eval (rctx, fctx) (ENot exp) = do
  (v1, fctx1) <- eval (rctx, fctx) exp
  return (ValorBool (not $ b v1), fctx1)
eval (rctx, fctx) (ECon exp0 exp) = do
  (v1, fctx1) <- eval (rctx, fctx) exp0
  (v2, fctx2) <- eval (rctx, fctx1) exp
  return (ValorStr (s v1 ++ s v2), fctx2)
eval (rctx, fctx) (EAdd exp0 exp) = do
  (v1, fctx1) <- eval (rctx, fctx) exp0
  (v2, fctx2) <- eval (rctx, fctx1) exp
  return (ValorInt (i v1 + i v2), fctx2)
eval (rctx, fctx) (ESub exp0 exp) = do
  (v1, fctx1) <- eval (rctx, fctx) exp0
  (v2, fctx2) <- eval (rctx, fctx1) exp
  return (ValorInt (i v1 - i v2), fctx2)
eval (rctx, fctx) (EMul exp0 exp) = do
  (v1, fctx1) <- eval (rctx, fctx) exp0
  (v2, fctx2) <- eval (rctx, fctx1) exp
  return (ValorInt (i v1 * i v2), fctx2)
eval (rctx, fctx) (EDiv exp0 exp) = do
  (v1, fctx1) <- eval (rctx, fctx) exp0
  (v2, fctx2) <- eval (rctx, fctx1) exp
  return (ValorInt (i v1 `div` i v2), fctx2)
eval (rctx, fctx) (ECall id lexp) = do
  (ValorFun funDef) <- lookup rctx id
  (args, fctx') <- evalArgs (rctx, fctx) lexp

  let funCall = FunCall id args
      parameterBindings = zip (getParams funDef) args
      contextFunctions = filter isFunction rctx
   in case lookupMemo fctx' funCall of
        Just rv -> return (rv, fctx')
        Nothing -> memoize funCall <$> eval (parameterBindings ++ contextFunctions, fctx') (getExp funDef)
  where
    evalArgs :: Context -> [Exp] -> Err ([Valor], FContext)
    evalArgs (_, fctx) [] = Ok ([], fctx)
    evalArgs (rctx, fctx) (e : es) = do
      (v, fctx1) <- eval (rctx, fctx) e
      (vs, fctx2) <- evalArgs (rctx, fctx1) es
      return (v : vs, fctx2)

    isFunction :: (Ident, Valor) -> Bool
    isFunction (_, ValorFun _) = True
    isFunction _ = False

    memoize :: FunCall -> (Valor, FContext) -> (Valor, FContext)
    memoize fc (rv, fctx) = (rv, updateMemo fctx fc rv)
eval (rctx, fctx) (EVar id) = do
  v <- lookup rctx id
  return (v, fctx)
eval (_, fctx) (EInt n) = Ok (ValorInt n, fctx)
eval (_, fctx) (EStr s) = Ok (ValorStr s, fctx)
eval (_, fctx) ETrue = Ok (ValorBool True, fctx)
eval (_, fctx) EFalse = Ok (ValorBool False, fctx)

lookup :: RContext -> Ident -> Err Valor
lookup [] id' = Bad ("@interpreter: " ++ printTree id' ++ " nao esta no contexto. ")
lookup ((id, v) : cs) id'
  | id == id' = Ok v
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
