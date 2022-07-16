module DPLL where

import Rewrite
import Symbols
import Data.List as List
import Control.Exception (throw, Exception)
import Data.Data (Typeable)
import Data.Graph (outdegree)
import Data.Maybe

findUnitClauses :: PLClause -> [Lit]
findUnitClauses (Clause c) = foldl (\acc x -> if length x == 1 then head x:acc else acc) [] c

removeElem :: Eq a => a -> [a] -> [a]
removeElem c = List.filter (c /=)
oppLit :: Lit -> Lit
oppLit (Pos c) = Neg c
oppLit (Neg c) = Pos c
oppLit CTrue = CFalse
oppLit CFalse = CTrue

unitPropagation :: PLClause -> Lit -> PLClause
unitPropagation (Clause  c) lit = Clause $ foldl (\acc x -> if lit `elem` x then acc else removeElem (oppLit lit) x : acc) [] c

setInsert :: Lit -> [Lit] -> [Lit]
setInsert CFalse set = set
setInsert CTrue set = set
setInsert x set = if x `elem` set then set else x:set
setUnion :: [Lit] -> [Lit] -> [Lit]
setUnion = List.foldr setInsert

getLiterals :: PLClause -> [Lit]
getLiterals (Clause c) = List.foldr setUnion [] c

pureLiterals :: PLClause -> [Lit]
pureLiterals c = filter (\x -> oppLit x `elem` literals) literals
       where literals = getLiterals c

data Unsatisfiable = Unsatisfiable deriving (Show, Typeable)
instance Exception Unsatisfiable

type Output = [(String, Bool)]

insertOutput :: Output -> Lit -> Output
insertOutput [] (Pos c) = [(c, Prelude.True)]
insertOutput [] (Neg c) = [(c, Prelude.False)]
insertOutput out (Pos c) = if ((c, Prelude.True) `elem` out)  || ((c, Prelude.False) `elem` out)
                           then out
                           else (c,Prelude.True):out
insertOutput out (Neg c) = if ((c, Prelude.True) `elem` out)  || ((c, Prelude.False) `elem` out)
                           then out
                           else (c,Prelude.False):out
insertOutput out _ = out



dpll :: PLClause -> Output -> Maybe Output
dpll (Clause [[]]) assignment = Nothing
dpll (Clause []) assignment = Just assignment
dpll clauses assignment = uncurry dpll step2
    where unitClauses = findUnitClauses clauses
          step2 = if not (null unitClauses)
                  then (List.foldl unitPropagation clauses unitClauses,
                        foldl insertOutput assignment unitClauses)
                  else step3
          pureLit = pureLiterals clauses
          step3 = if not (null pureLit)
                  then (List.foldl unitPropagation clauses unitClauses,
                        foldl insertOutput assignment pureLit)
                  else step4
          literals = getLiterals clauses
          choice = name $ head literals
          Clause c = clauses
          output1 = dpll (Clause ([Pos choice]:c)) ((choice, Prelude.True):assignment)
          step4 
            | isNothing output1 = step5
            | otherwise         = (Clause [], fromJust output1)
          output2 = dpll (Clause ([Neg choice]:c)) ((choice, Prelude.False):assignment)
          step5 
            | isNothing output2 = (Clause [[]], assignment)
            | otherwise         = (Clause [], fromJust output2)


outputToString :: Maybe Output -> String
outputToString Nothing = "There is no satisfiable assignment"
outputToString (Just x) = List.foldr (\(c,t) out -> 
                        if t 
                        then c ++ ":T\n" ++ out 
                        else c ++ ":F\n" ++ out) "" x