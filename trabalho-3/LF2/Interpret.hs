module Main where

import AbsLF
import LexLF
import ParLF
import PrintLF
import ErrM

import Interpreter
import Optimizer
import Typechecker

main :: IO ()
main = do
  interact calc
  putStrLn ""

calc :: [Char] -> String
calc sourceCode = either id showProgram (runProgram sourceCode)

runProgram :: [Char] -> Err (Program, Program, Valor)
runProgram sourceCode = do
  ast <- pProgram (myLexer sourceCode)
  opt <-
    let typeCheckResult = typeCheckP ast
     in if any isError typeCheckResult
          then Bad (show $ filter isError typeCheckResult)
          else Ok (optimizeP ast)

  return (ast, opt, executeP opt)

showProgram :: (Program, Program, Valor) -> String
showProgram (ast, opt, rv) =
  ">>>>>>> Programa original:<<<<<<< \n"
    ++ printTree ast
    ++ "\n"
    ++ ">>>>>>> Programa otimizado:<<<<<<< \n"
    ++ printTree opt
    ++ "\n"
    ++ ">>>>>>> Resultado da execucao:<<<<<<< \n"
    ++ show rv
