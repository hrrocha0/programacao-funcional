module Typechecker where

import Prelude hiding (lookup)

import AbsLF
import PrintLF

data R a = OK a | Erro String
  deriving (Eq, Ord, Show, Read)

isError :: R a -> Bool
isError e = case e of
  OK _ -> False
  Erro _ -> True

type TContext = [(Ident, Type)]

typeCheckP :: Program -> [R TContext]
typeCheckP (Prog fs) =
  let nCtx = updatecF [] fs
   in case nCtx of
        OK ctx -> map (typeCheckF ctx) fs
        Erro msg -> [Erro msg]

typeCheckF :: TContext -> Function -> R TContext
typeCheckF tc (Fun tR _ decls exp) = tke (parameterTypeBindings ++ functionTypes) exp tR
  where
    parameterTypeBindings = map (\(Dec tp id) -> (id, tp)) decls
    functionTypes =
      filter
        ( \(i, t) -> case t of
            TFun _ _ -> True
            _ -> False
        )
        tc

tke :: TContext -> Exp -> Type -> R TContext
tke tc exp tp =
  let r = tinf tc exp
   in case r of
        OK tipo ->
          if tipo == tp
            then OK tc
            else
              Erro
                ( "@typechecker: a expressao "
                    ++ printTree exp
                    ++ " tem tipo "
                    ++ printTree tipo
                    ++ " mas o tipo esperado eh "
                    ++ printTree tp
                )
        Erro msg -> Erro msg

{-
A lógica da verificação de tipos daa expressão ECall na LF2 é a mesma da LI2 tipada. Isso ocorre porque
a verificação de tipos depende apenas do tipo de retorno e de seus parâmetros, que funcionam de maneira
semelhante nas duas linguagens.
-}
tinf :: TContext -> Exp -> R Type
tinf tc x = case x of
  ECon exp0 exp -> combChecks tc exp0 exp TStr
  EAdd exp0 exp -> combChecks tc exp0 exp Tint
  ESub exp0 exp -> combChecks tc exp0 exp Tint
  EMul exp0 exp -> combChecks tc exp0 exp Tint
  EDiv exp0 exp -> combChecks tc exp0 exp Tint
  EOr exp0 exp -> combChecks tc exp0 exp Tbool
  EAnd exp0 exp -> combChecks tc exp0 exp Tbool
  ENot exp ->
    let r = tke tc exp Tbool
     in case r of
          OK _ -> OK Tbool
          Erro msg -> Erro msg
  EStr str -> OK TStr
  ETrue -> OK Tbool
  EFalse -> OK Tbool
  EInt n -> OK Tint
  EVar id -> lookup tc id
  eIf@(EIf exp expT expE) ->
    let r = tke tc exp Tint
     in case r of
          OK _ ->
            let r2 = tinf tc expT
             in case r2 of
                  OK t ->
                    let r3 = tke tc expE t
                     in case r3 of
                          OK _ -> r2
                          Erro msg -> Erro msg
                  Erro msg -> Erro msg
          Erro msg -> Erro msg
  ECall id lexp ->
    let r = lookup tc id
     in case r of
          OK (TFun tR pTypes) ->
            if length pTypes == length lexp
              then
                if isThereError tksArgs /= []
                  then Erro " @typechecker: chamada de funcao invalida"
                  else OK tR
              else Erro " @typechecker: tamanhos diferentes de lista de argumentos e parametros"
            where
              tksArgs = zipWith (tke tc) lexp pTypes
              isThereError l =
                filter
                  not
                  ( map
                      ( \e ->
                          ( let r2 = e
                             in case r2 of
                                  OK _ -> True
                                  Erro _ -> False
                          )
                      )
                      l
                  )
          Erro msg -> Erro msg

combChecks :: TContext -> Exp -> Exp -> Type -> R Type
combChecks tc exp1 exp2 tp =
  let r = tke tc exp1 tp
   in case r of
        OK _ ->
          let r2 = tke tc exp2 tp
           in case r2 of
                OK _ -> OK tp
                Erro msg -> Erro msg
        Erro msg -> Erro msg

lookup :: TContext -> Ident -> R Type
lookup [] id = Erro ("@typechecker: " ++ printTree id ++ " nao esta no contexto. ")
lookup ((id, value) : cs) key
  | id == key = OK value
  | otherwise = lookup cs key

updateTC :: TContext -> Ident -> Type -> R TContext
updateTC [] id tp = OK [(id, tp)]
updateTC ((id, tp) : idTps) idN tpN
  | id == idN = Erro ("@typechecker: identificador" ++ printTree id ++ " nao pode ter mais de um tipo")
  | otherwise =
      let r = updateTC idTps idN tpN
       in case r of
            OK restOK -> OK ((id, tp) : restOK)
            Erro msg -> Erro msg

getFunctionType :: Function -> Type
getFunctionType (Fun tipoRetorno _ decls _) = TFun tipoRetorno (map (\(Dec tp _) -> tp) decls)

updatecF :: TContext -> [Function] -> R TContext
updatecF tc [] = OK tc
updatecF tc (f@(Fun _ nomeF _ _) : fs) =
  let r = updateTC tc nomeF (getFunctionType f)
   in case r of
        OK tcNew -> updatecF tcNew fs
        Erro msg -> Erro msg
