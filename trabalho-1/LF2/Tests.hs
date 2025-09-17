module Tests where

import AbsLF

{-
int double (int x)
{
  x + x
}
int quadruple (int y)
{
  double (double (y))
}
int main ()
{
  quadruple (3)
}
-}
test1 :: Program
test1 = Prog [Fun Tint (Ident "double") [Dec Tint (Ident "x")] (EAdd (EVar (Ident "x")) (EVar (Ident "x"))), Fun Tint (Ident "quadruple") [Dec Tint (Ident "y")] (ECall (Ident "double") [ECall (Ident "double") [EVar (Ident "y")]]), Fun Tint (Ident "main") [] (ECall (Ident "quadruple") [EInt 3])]

-------------------------------------------------------

{-
int sum (int n)
{
  if (n) then n + sum (n - 1) else 0
}
int main ()
{
  sum (10)
}
-}
test2 :: Program
test2 = Prog [Fun Tint (Ident "sum") [Dec Tint (Ident "n")] (EIf (EVar (Ident "n")) (EAdd (EVar (Ident "n")) (ECall (Ident "sum") [ESub (EVar (Ident "n")) (EInt 1)])) (EInt 0)), Fun Tint (Ident "main") [] (ECall (Ident "sum") [EInt 10])]

----------------------------------------------------------

{-
int main ()
{
  fat (5)
}
int fat (int n)
{
  if (n) then n * fat (n - 1) else 1
}
-}
test3 :: Program
test3 = Prog [Fun Tint (Ident "main") [] (ECall (Ident "fat") [EInt 5]), Fun Tint (Ident "fat") [Dec Tint (Ident "n")] (EIf (EVar (Ident "n")) (EMul (EVar (Ident "n")) (ECall (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)])) (EInt 1))]

{- Entradas: int x
int main () {
    fib (10)
}
int fib (int n) {
    if (n)
        then if (n - 1)
            then fib (n - 1) + fib (n - 2)
            else 1
        else 1
}
-}
test4 :: Integer -> Program
test4 x = Prog [Fun Tint (Ident "main") [] (ECall (Ident "fib") [EInt x]), Fun Tint (Ident "fib") [Dec Tint (Ident "n")] (EIf (EVar (Ident "n")) (EIf (ESub (EVar (Ident "n")) (EInt 1)) (EAdd (ECall (Ident "fib") [ESub (EVar (Ident "n")) (EInt 1)]) (ECall (Ident "fib") [ESub (EVar (Ident "n")) (EInt 2)])) (EInt 1)) (EInt 1))]

{- Entradas: String s
String main () {
    greet ("LF2")
}
String greet (String name) {
    if (1)
        then "Hello, " ++ name ++ "!"
        else "ERROR"
}
-}
test5 :: String -> Program
test5 s = Prog [Fun TStr (Ident "main") [] (ECall (Ident "greet") [EStr s]), Fun TStr (Ident "greet") [Dec TStr (Ident "name")] (EIf (EInt 1) (ECon (ECon (EStr "Hello, ") (EVar (Ident "name"))) (EStr "!")) (EStr "ERROR"))]
