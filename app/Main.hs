module Main where

import Data.List
import DPLL
import Symbols
import Rewrite


main :: IO ()
main = do
    putStrLn $ toString testFormula
    putStrLn $ toString $ nnf testFormula
    putStrLn $ toString $ cnf testFormula
    putStrLn $ toString $ dnf testFormula
    putStrLn $ toString $ cnfClauses testFormula
    putStrLn $ toString $ unitPropagation testClause (Pos "2")