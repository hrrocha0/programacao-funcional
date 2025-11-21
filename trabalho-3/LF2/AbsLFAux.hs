module AbsLFAux where

import AbsLF

getName :: Function -> Ident
getName (Fun _ name _ _) = name

getParams :: Function -> [Ident]
getParams (Fun _ _ params _) = map (\(Dec _ ident) -> ident) params

getExp :: Function -> Exp
getExp (Fun _ _ _ exp) = exp
