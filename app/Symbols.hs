module Symbols where
import Data.Set as Set
import Data.List as List

class Literal a where
    litName :: a -> String

class Representation a  where
    toString :: a ->String

-- Propositional logic terms
data PLTerm = True | False | Lit String| Not PLTerm | And PLTerm PLTerm | Or PLTerm PLTerm  | Impl PLTerm PLTerm deriving (Eq, Ord)

instance Representation PLTerm where
    toString (Lit c) = c
    toString (And a b) = "(" ++ toString a ++ " & " ++ toString b ++ ")"
    toString (Or a b) = "(" ++ toString a ++ " V " ++ toString b ++ ")"
    toString (Not a) =  "~" ++ toString a
    toString (Impl a b) =  "(" ++ toString a ++ " -> " ++ toString b ++ ")"
    toString Symbols.True = "T"
    toString Symbols.False = "F"

-- Propositional logic clause term
data Lit = CTrue | CFalse | Pos {name :: String} | Neg {name :: String} deriving (Eq, Ord)
newtype PLClause = Clause [[Lit]]

litCName CTrue = "T"
litCName CFalse = "F"
litCName (Pos c) = c
litCName (Neg c) = '~':c

clauseToString :: [Lit] -> String
clauseToString = (++ "}") . ("{" ++) . intercalate "," . List.map litCName
instance Representation PLClause where
    toString (Clause c) = concatMap clauseToString c









testFormula = Not(And (Impl (Lit "x") (Lit "y")) (Or (Lit "z") (Lit "x")))
testClause = Clause [[Pos "1", Neg "2", Neg "3"],[Pos "2", Neg "3", Pos "4"],[Pos "1", Pos "3", Neg "4"]]