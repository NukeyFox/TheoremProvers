module DPLL where

import Rewrite
import Symbols
import Data.List
import Data.HashSet
import Control.Applicative (Alternative(empty))

findUnitClauses :: PLClause -> [Lit]
findUnitClauses (Clause c) = foldl (\acc x -> if length x == 1 then head x:acc else acc) [] c


removeElem c = List.filter (c /=)
oppLit (Pos c) = Neg c
oppLit (Neg c) = Pos c
oppLit CTrue = CFalse
oppLit CFalse = CTrue

unitPropagation :: PLClause -> Lit -> PLClause
unitPropagation (Clause  c) lit = Clause $ foldl (\acc x -> if lit `elem` x then acc else removeElem (oppLit lit) x : acc) [] c

getLiterals :: PLClause -> HashSet Lit
getLiterals (Clause []) = HashSet.empty
getLiterals (Clause (x:xs)) = List.map (swap HashSet.insert (getLiterals xs)) x  
