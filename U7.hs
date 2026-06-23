import Data.List (intercalate)

-- Vorgegeben
data Literal = Atom { atomChar :: Char }
             | NegA { atomChar :: Char }
             deriving Eq

instance Show Literal where
    show (Atom a) = [a]
    show (NegA a) = ['!', a]

data Clause = Clause [Literal]
data CNF    = CNF [Clause]


-- Aufgabe 1
instance Show Clause where
    show (Clause lits) = "(" ++ intercalate " v " (map show lits) ++ ")"

instance Show CNF where
    show (CNF clauses) = intercalate " ^ " (map show clauses)


-- Aufgabe 2
fromClause :: Clause -> [Literal]
fromClause (Clause lits) = lits

fromCNF :: CNF -> [Clause]
fromCNF (CNF clauses) = clauses


-- Aufgabe 3
alphaL :: [Literal] -> Literal -> Bool
alphaL lits lit = lit `elem` lits


-- Aufgabe 4
alphaC :: (Literal -> Bool) -> Clause -> Bool
-- Klausel = Disjunktion → min. ein Literal muss True sein
alphaC alpha (Clause lits) = any alpha lits
-- any: mind. ein Element erfuellt Bedingung

alphaCNF :: (Literal -> Bool) -> CNF -> Bool
-- CNF = Konjunktion → alle Klauseln muessen True sein
alphaCNF alpha (CNF clauses) = all (alphaC alpha) clauses
-- all: alle Elemente erfuellen Bedingung


-- Aufgabe 5

a :: Clause
a = Clause [NegA 'A', Atom 'B']

b :: Clause
b = Clause [NegA 'B', Atom 'C']

c :: CNF
c = CNF [a, b]



ergebnisse :: [Bool]
-- Alle 8 Belegungen auf CNF c anwenden
ergebnisse = map (\lits -> alphaCNF (alphaL lits) c)
    [[x, y, z] | x <- [Atom 'A', NegA 'A']  -- A oder !A
               , y <- [Atom 'B', NegA 'B']  -- B oder !B
               , z <- [Atom 'C', NegA 'C']] -- C oder !C


-- Aufgabe 6
isHornClause :: Clause -> Bool
-- max. 1 positives Literal pro Klausel
isHornClause (Clause lits) = length (filter isAtom lits) <= 1
    where isAtom (Atom _) = True  -- positives Literal
          isAtom (NegA _) = False -- negatives Literal

isHorn :: CNF -> Bool
-- alle Klauseln muessen Horn sein
isHorn (CNF clauses) = all isHornClause clauses