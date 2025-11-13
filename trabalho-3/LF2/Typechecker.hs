module Typechecker where

import AbsLF
import PrintLF
import ErrM

import Data.Either
import Prelude hiding (lookup)

type TContext = [(Ident, Type)]

isError :: Err a -> Bool
isError = isLeft

typeCheckP :: Program -> [Err TContext]
typeCheckP (Prog fs) = case updatecF [] fs of
  Ok ctx -> map (typeCheckF ctx) fs
  Bad errorMessage -> [Bad errorMessage]

typeCheckF :: TContext -> Function -> Err TContext
typeCheckF tc (Fun rt _ decls exp) =
  let parameterTypeBindings = map (\(Dec tp id) -> (id, tp)) decls
      functionTypes = filter isFunction tc
   in tke (parameterTypeBindings ++ functionTypes) exp rt
  where
    isFunction :: (Ident, Type) -> Bool
    isFunction (_, TFun _ _) = True
    isFunction _ = False

tke :: TContext -> Exp -> Type -> Err TContext
tke tc exp t = do
  t' <- tinf tc exp

  if t == t'
    then Ok tc
    else
      Bad
        ( "@typechecker: a expressao "
            ++ printTree exp
            ++ " tem tipo "
            ++ printTree t'
            ++ " mas o tipo esperado eh "
            ++ printTree t
        )

tinf :: TContext -> Exp -> Err Type
tinf tc (EIf expC expT expE) = do
  t <- tinf tc expT
  tke tc expC Tint
  tke tc expE t
  return t
tinf tc (EOr exp0 exp) = combChecks tc exp0 exp Tbool
tinf tc (EAnd exp0 exp) = combChecks tc exp0 exp Tbool
tinf tc (ENot exp) = do
  tke tc exp Tbool
  return Tbool
tinf tc (ECon exp0 exp) = combChecks tc exp0 exp TStr
tinf tc (EAdd exp0 exp) = combChecks tc exp0 exp Tint
tinf tc (ESub exp0 exp) = combChecks tc exp0 exp Tint
tinf tc (EMul exp0 exp) = combChecks tc exp0 exp Tint
tinf tc (EDiv exp0 exp) = combChecks tc exp0 exp Tint
tinf tc (ECall id lexp) = do
  (TFun rt ts) <- lookup tc id

  let tksArgs = zipWith (tke tc) lexp ts
   in if length ts == length lexp
        then
          if any isError tksArgs
            then Bad " @typechecker: chamada de funcao invalida"
            else Ok rt
        else Bad "@typechecker: tamanhos diferentes de lista de argumentos e parametros"
tinf tc (EInt _) = Ok Tint
tinf tc (EVar id) = lookup tc id
tinf tc (EStr _) = Ok TStr
tinf tc ETrue = Ok Tbool
tinf tc EFalse = Ok Tbool

combChecks :: TContext -> Exp -> Exp -> Type -> Err Type
combChecks tc exp1 exp2 t = do
  tke tc exp1 t
  tke tc exp2 t
  return t

lookup :: TContext -> Ident -> Err Type
lookup [] i' = Bad ("@typechecker: " ++ printTree i' ++ " nao esta no contexto. ")
lookup ((i, v) : cs) i'
  | i == i' = Ok v
  | otherwise = lookup cs i'

updateTC :: TContext -> Ident -> Type -> Err TContext
updateTC [] i' v' = Ok [(i', v')]
updateTC ((i, v) : cs) i' v'
  | i == i' = Bad ("@typechecker: identificador" ++ printTree i ++ " nao pode ter mais de um tipo")
  | otherwise = do
      cs' <- updateTC cs i' v'
      return ((i, v) : cs')

updatecF :: TContext -> [Function] -> Err TContext
updatecF tc [] = Ok tc
updatecF tc (f@(Fun _ id _ _) : fs) = do
  tc' <- updateTC tc id (getFunctionType f)
  updatecF tc' fs
  where
    getFunctionType :: Function -> Type
    getFunctionType (Fun rt _ decls _) = TFun rt (map (\(Dec t _) -> t) decls)
