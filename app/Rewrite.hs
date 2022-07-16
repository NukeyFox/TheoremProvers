module Rewrite where

import Symbols
import Text.Read (Lexeme(Symbol))

-- negation of different literals
neg :: PLTerm -> PLTerm
neg (Lit n)   = Not (Lit n)
neg (Not a)   = a
neg (And a b) = Or  (neg a) (neg b)
neg (Or a b)  = And (neg a) (neg b)
neg (Impl a b)= And a (neg b)
neg Symbols.True  = Symbols.False
neg Symbols.False = Symbols.True

nnf :: PLTerm -> PLTerm
nnf Symbols.True = Symbols.True
nnf Symbols.False = Symbols.False
nnf (Lit n) = Lit n
nnf (Not a) = neg a
nnf (And a b) = And (nnf a) (nnf b)
nnf (Or a b) = Or (nnf a) (nnf b)
nnf (Impl a b) = Or (neg $ nnf a) (nnf b)

cnf = distrAnd . nnf
    where distrAnd (Or a (And b c)) = And (distrAnd (Or a b)) (distrAnd (Or a c))
          distrAnd (Or (And a b) c) = And (distrAnd (Or a c)) (distrAnd (Or b c))
          distrAnd (Or a b) = if a == b then distrAnd a else Or (distrAnd a) (distrAnd b)
          distrAnd (And a b) = if a == b then distrAnd a else And (distrAnd a) (distrAnd b)
          distrAnd (Not a) = Not (distrAnd a)
          distrAnd x = x

dnf = distrOr . nnf
    where distrOr (And a (Or b c)) = And (distrOr (Or a b)) (distrOr (Or a c))
          distrOr (And (Or a b) c) = And (distrOr (Or a c)) (distrOr (Or b c))
          distrOr (And a b) = if a == b then distrOr a else And (distrOr a) (distrOr b)
          distrOr (Or a b) = if a == b then distrOr a else Or (distrOr a) (distrOr b)
          distrOr (Not a) = Not (distrOr a)
          distrOr x = x


cnfClauses :: PLTerm -> PLClause
cnfClauses = Clause . toClause . cnf
    where toClause Symbols.True = [[Symbols.CTrue]]
          toClause Symbols.False = [[Symbols.CFalse]]
          toClause (Not (Lit a)) = [[Neg a]]
          toClause (Lit a)       = [[Pos a]]
          toClause (Or a b)      = [concat (toClause a ++ toClause b)]
          toClause (And a b)     = toClause a ++ toClause b
          toClause (Impl a b)    = toClause (Or (neg a) b)
          toClause _       = []
