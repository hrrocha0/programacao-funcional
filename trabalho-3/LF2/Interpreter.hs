module Interpreter where

import AbsLF
import AbsLFAux
import PrintLF
import ErrM

import Control.Monad.State
import Data.List hiding (lookup)
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

type Context a = [(a, Valor)]

type RContext = Context Ident

type FContext = Context FunCall

type EvalResult = State FContext Valor

executeP :: Program -> Err Valor
executeP (Prog fs) = case find (\f -> getName f == Ident "main") fs of
  Just f ->
    let ctx = updatecF [] fs
        exp = getExp f
     in Ok $ evalState (eval ctx exp) []
  Nothing -> Bad "@interpreter: a funcao main nao foi encontrada"

eval :: RContext -> Exp -> EvalResult
eval ctx (EIf expC expT expE) = do
  v1 <- i <$> eval ctx expC

  if v1 /= 0
    then eval ctx expT
    else eval ctx expE
eval ctx (EOr exp0 exp) = do
  v1 <- b <$> eval ctx exp0
  v2 <- b <$> eval ctx exp
  return $ ValorBool (v1 || v2)
eval ctx (EAnd exp0 exp) = do
  v1 <- b <$> eval ctx exp0
  v2 <- b <$> eval ctx exp
  return $ ValorBool (v1 && v2)
eval ctx (ENot exp) = do
  v1 <- b <$> eval ctx exp
  return $ ValorBool (not v1)
eval ctx (ECon exp0 exp) = do
  v1 <- s <$> eval ctx exp0
  v2 <- s <$> eval ctx exp
  return $ ValorStr (v1 ++ v2)
eval ctx (EAdd exp0 exp) = do
  v1 <- i <$> eval ctx exp0
  v2 <- i <$> eval ctx exp
  return $ ValorInt (v1 + v2)
eval ctx (ESub exp0 exp) = do
  v1 <- i <$> eval ctx exp0
  v2 <- i <$> eval ctx exp
  return $ ValorInt (v1 - v2)
eval ctx (EMul exp0 exp) = do
  v1 <- i <$> eval ctx exp0
  v2 <- i <$> eval ctx exp
  return $ ValorInt (v1 * v2)
eval ctx (EDiv exp0 exp) = do
  v1 <- i <$> eval ctx exp0
  v2 <- i <$> eval ctx exp
  return $ ValorInt (v1 `div` v2)
eval ctx (ECall id lexp) = do
  args <- evalArgs ctx lexp
  fctx <- get

  let (Just funDef) = f <$> lookup ctx id
      funCall = FunCall id args
      paramBindings = zip (getParams funDef) args
      contextFunctions = filter isFunction ctx
   in case lookup fctx funCall of
        Just v -> return v
        Nothing -> eval (paramBindings ++ contextFunctions) (getExp funDef) >>= memoize funCall
  where
    evalArgs :: RContext -> [Exp] -> State FContext [Valor]
    evalArgs _ [] = return []
    evalArgs ctx (e : es) = do
      v <- eval ctx e
      vs <- evalArgs ctx es
      return (v : vs)

    isFunction :: (Ident, Valor) -> Bool
    isFunction (_, ValorFun _) = True
    isFunction _ = False

    memoize :: FunCall -> Valor -> State FContext Valor
    memoize fc v = do
      modify (\fctx -> update fctx fc v)
      return v
eval ctx (EVar id) = let (Just v) = lookup ctx id in return v
eval _ (EInt n) = return $ ValorInt n
eval _ (EStr s) = return $ ValorStr s
eval _ ETrue = return $ ValorBool True
eval _ EFalse = return $ ValorBool False

lookup :: (Eq a) => Context a -> a -> Maybe Valor
lookup [] _ = Nothing
lookup ((k, v) : cs) k'
  | k == k' = Just v
  | otherwise = lookup cs k'

update :: (Eq a) => Context a -> a -> Valor -> Context a
update [] k' v' = [(k', v')]
update ((k, v) : cs) k' v'
  | k == k' = (k', v') : cs
  | otherwise = (k, v) : update cs k' v'

updatecF :: RContext -> [Function] -> RContext
updatecF = foldl (\ctx f -> update ctx (getName f) (ValorFun f))
