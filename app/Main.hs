module Main where

import System.IO
import Parser

main :: IO ()
main =
  do putStr "Enter an arithmetic expression:"
     hFlush stdout
     expression <- getLine
     let result = Parser.expr expression
     print . fst . head $ result
