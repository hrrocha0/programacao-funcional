module Main where

import Interpreter
import Optimizer
import Typechecker

import AbsLF
import LexLF
import ParLF
import PrintLF
import ErrM

main :: IO ()
main = do
  interact calc
  putStrLn ""

calc :: [Char] -> String
calc sourceCode =
  let parserResult = pProgram (myLexer sourceCode)
   in case parserResult of
        Ok ast ->
          let typeCheckResult = typeCheckP ast
           in if any isError typeCheckResult
                then show (filter isError typeCheckResult)
                else
                  let optProgram = optimizeP ast
                   in ">>>>>>> Programa original:<<<<<<< \n"
                        ++ printTree ast
                        ++ "\n"
                        ++ ">>>>>>> Programa otimizado:<<<<<<< \n"
                        ++ printTree optProgram
                        ++ "\n"
                        ++ ">>>>>>> Resultado da execucao:<<<<<<< \n"
                        ++ show (executeP optProgram)
        Bad errorMessage -> errorMessage
