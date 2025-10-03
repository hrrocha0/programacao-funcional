{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}

module Interpreter where

import AbsLF

-- TODO: leia agora o conteudo desse arquivo (AbsLFAux.hs) e explique por que refatoramos assim
{-
  As funções definidas em AbsLFAux.hs foram retiradas deste arquivo pois estão relacionadas diretamente à 
  sintaxe abstrata da linguagem, e não a como ela é interpretada. Por isso, o conteúdo do novo arquivo é 
  muito mais sensivel a mudanças na sintaxe concreta da linguagem.
-}
import AbsLFAux

import Prelude hiding (lookup)

executeP :: Program -> Valor
executeP (Prog fs) = eval (updatecF [] fs) (expMain fs)
  where
    expMain (f : xs)
      | getName f == Ident "main" = getExp f
      | otherwise = expMain xs

type RContext = [(Ident, Valor)]

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
  -- TODO: na linha abaixo, retorne um ValorFun contendo o lambda e saiba explicar a razao
  {-
    A evaluação de uma expressão Elambda retorna um valor contendo a si própria porque sua
    aplicaão depende dos argumentos fornecidos em sua chamada pela expressão ECall.
  -}
  lambda@(ELambda params exp) -> ValorFun lambda
  -- TODO: em EComp abaixo, troque undefined (2 ocorrencias) pela construcao apropriada
  EComp exp1 exp2 ->
    let (ValorFun exp1') = eval context exp1
        (ValorFun exp2') = eval context exp2
     in ValorFun
          ( ELambda
              (getParamsTypesL exp2')
              (ECall exp1' [ECall exp2' (getParamsExpL exp2')])
          )
  {- 
    TODO: em ECall abaixo, troque undefined (3 ocorrencias) pela construcao apropriada.
    Dica: estude o codigo, buscando entender tambem as definicoes locais
  -}
  ECall exp lexp ->
    if length lexp < length parameters
      -- TODO: que caso eh esse ?
      {-
        Esse caso ocorre quando há aplicação parcial da função e retorna uma função
        lambda para os parâmetros restantes.
      -}
      then ValorFun (ELambda params' exp')
      -- TODO: que caso eh esse ?
      {-
        Esse caso ocorre quando há aplicação total da função e retorna o valor da
        evaluação da expressão exp', definida abaixo.
      -}
      else eval (paramBindings ++ contextFunctions) exp'
    where
      (ValorFun lambda) = eval context exp
      parameters = getParamsL lambda
      paramBindings = zip parameters (map (eval context) lexp)
      {-
        A definição abaixo contém os parâmetros que não foram fornecidos na aplicação
        parcial da função.
      -}
      params' = drop (length lexp) (getParamsTypesL lambda)
      {-
        A expressão abaixo é resultado da substituição das ocorrências dos parâmetros
        pelos valores dos argumentos fornecidos na chamadas da função. A implementação
        foi feita dessa forma porque o retorno da função pode ser uma função lambda.
      -}
      exp' = subst paramBindings (getExpL lambda)
      contextFunctions =
        filter
          ( \(i, v) -> case v of
              ValorFun _ -> True
              _ -> False
          )
          context

-- a função "subst" gera uma nova expressao a partir dos bindings em RContext -- Linha 59
subst :: RContext -> Exp -> Exp
subst rc exp = case exp of
  -- TODO: por que eh implementado assim ?
  {-
    A implementação é feita dessa forma porque é possível que o contexto não
    forneça um valor para a variável .
  -}
  EVar id -> bind id rc -- Linha 62
  -- TODO: explique a implementacao da linha abaixo
  {-
    A linha abaixo substitui os valores da expressão exp, com exceção daqueles que
    são parâmetros da expressão lambda.
  -}
  lambda@(ELambda paramsTypes exp) -> ELambda paramsTypes (subst (rc `diff` getParamsL lambda) exp) -- Linha 64
  ECall exp lexp -> ECall (subst rc exp) (map (subst rc) lexp) -- Linha 65
  EAdd exp0 exp -> EAdd (subst rc exp0) (subst rc exp) -- Linha 66
  -- TODO: nos casos abaixo, troque cada undefined pela construcao apropriada
  EComp exp1 exp2 -> EComp (subst rc exp1) (subst rc exp2)
  EIf expC expT expE -> EIf (subst rc expC) (subst rc expT) (subst rc expE)
  ECon exp0 exp -> ECon (subst rc exp0) (subst rc exp)
  ESub exp0 exp -> ESub (subst rc exp0) (subst rc exp)
  EMul exp0 exp -> EMul (subst rc exp0) (subst rc exp)
  EDiv exp0 exp -> EDiv (subst rc exp0) (subst rc exp)
  EOr exp0 exp -> EOr (subst rc exp0) (subst rc exp)
  EAnd exp0 exp -> EAnd (subst rc exp0) (subst rc exp)
  ENot exp -> ENot (subst rc exp)
  -- TODO: quais sao esses casos e por que sao implementados assim ?
  {-
    Os casos restantes são quando exp é um primitivo, logo não contém nenhuma variável e não há
    nenhuma substituição a ser feita.
  -}
  _ -> exp

{- 
  TODO: sobre a implementacao finalizada de subst:
    1)  qual eh o caso base?
    2)  como descrever o numero de casos recursivos? depende (in)diretamente de algo?
    3)  qual a finalidade dos casos recursivos?
    4)  por que a linha 64 eh diferente dos outros casos recursivos?
    5)  numa especificacao textual intuitiva e concisa (semelhante ao comentario na linha 59),
        qual a linha mais importante entre 62-77 ?
    6)  Ha semelhanca de implementacao em relacao ao Optimizer.hs? Qual(is)?
-}
{-
  1)  O caso base é quando exp é EVar.
  2)  O número de casos recursivos depende do tipo de expressão de exp.
  3)  A finalidade dos casos recursivos é realizar a substituição das ocorrências em expressões 
      mais complexas.
  4)  A linha 64 é diferente das demais porque uma expressão lambda contém parâmetros que não devem
      ser substituídos.
  5)  Entre as linhas 62 e 67, a mais importante é a linha 62: faz o bind de uma variável em RContext.
  6)  A implementação recursiva é semelhante à função optimizeE do arquivo Optimizer.hs quando exp é
      ECall, ELambda ou EComp.
-}

-- a função "diff" faz a diferença, tirando de RContext os mapeamentos envolvendo [Ident].
diff :: RContext -> [Ident] -> RContext
rc `diff` [] = rc
[] `diff` _ = []
((k, v) : kvs) `diff` (id : ids)
  | k == id = kvs `diff` ids
  | otherwise = (k, v) : (kvs `diff` (id : ids))

-- a função bind retorna uma expressao contendo o valor do id no RContext, ou o proprio id.
-- TODO: por que nao usamos o lookup no lugar de bind ?
{-
  A função bind é usada porque é possível que exp seja um parâmetro de uma função lambda. 
  Nesse caso, o id não estaria ligado em RContext e a função lookup resultaria em erro.
-}
bind :: Ident -> RContext -> Exp
bind id [] = EVar id -- retorna o proprio id se ele nao esta ligado em RContext
bind id ((k, v) : kvs)
  | k == id = wrapValueExpression v
  | otherwise = bind id kvs

-- "wrapValueExpression" empacota um valor em uma expressao
wrapValueExpression :: Valor -> Exp
wrapValueExpression (ValorInt i) = EInt i
wrapValueExpression (ValorStr s) = EStr s
wrapValueExpression (ValorBool True) = ETrue
wrapValueExpression (ValorBool False) = EFalse
wrapValueExpression (ValorFun exp) = exp

data Valor
  = ValorInt
      { i :: Integer
      }
  | ValorFun
      { f :: Exp -- f :: Function  **NOVO TODO: Por que mudou ?
      }
  | ValorStr
      { s :: String
      }
  | ValorBool
      { b :: Bool
      }

instance Show Valor where
  show :: Valor -> String
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  -- TODO: por que essa linha funciona?
  {-
    A linha abaixo funciona porque f é uma expressão do tipo Exp, que contém
    uma implementação base da função show (através da palavra-chave deriving).
  -}
  show (ValorFun f) = show f
lookup :: RContext -> Ident -> Valor
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> Ident -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv

-- TODO: explique a mudanca em updatecF
{-
  A função updatecF foi alterada porque ValorFun foi modificado para suportar
  expressões lambda. Agora, contém uma lambda no lugar de uma função.
-}
updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF c (f : fs) =
  updatecF
    ( update
        c
        (getName f)
        (ValorFun (ELambda (getParams f) (getExp f)))
    )
    fs

-- updatecF c (f:fs) = updatecF (update c (getName f) (ValorFun f)) fs
