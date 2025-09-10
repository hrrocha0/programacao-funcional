module Main where

import Interpreter

import AbsLF
import LexLF
import ParLF
import ErrM

main :: IO ()
main = do
  interact calc
  putStrLn ""

calc :: String -> String
calc s = case pProgram (myLexer s) of
  Ok p -> show (executeP p)
  Bad s -> show s
