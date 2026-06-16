import Data.Map qualified as Map
import Data.List (nub, intercalate)

data Symbol = N String | T String | Epsilon deriving (Ord)

type Production = [Symbol]

data Rule = Rule { lhs :: Symbol, rhs :: [Production] }

data Grammar = Grammar [Rule]

instance Eq Symbol where
    (==) (N n1) (N n2) = n1 == n2
    (==) (T t1) (T t2) = t1 == t2
    (==) Epsilon Epsilon = True
    (==) _ _ = False

-- Aufgabe 1
instance Show Symbol where
    show (N n)   = "<" ++ n ++ ">"
    show (T t)   = t
    show Epsilon = "epsilon"

-- Aufgabe 2
showProduction :: Production -> String
showProduction prod = "[" ++ intercalate ", " (map show prod) ++ "]"

showProductions :: [Production] -> String
showProductions prods = intercalate " | " (map showProduction prods)

instance Show Rule where
    show r = show (lhs r) ++ " -> " ++ showProductions (rhs r)

-- Aufgabe 3
instance Show Grammar where
    show (Grammar rules) = unlines (map show rules)

-- Aufgabe 4
removeEpsilon :: [Symbol] -> [Symbol]
removeEpsilon []           = []
removeEpsilon (Epsilon:xs) = removeEpsilon xs
removeEpsilon (x:xs)       = x : removeEpsilon xs

-- Aufgabe 5
containsEpsilon :: [Symbol] -> Bool
containsEpsilon []          = False
containsEpsilon (Epsilon:_) = True
containsEpsilon (_:xs)      = containsEpsilon xs

-- Aufgabe 6        sucht Produktionen die zu bestimmmten nichtterminal gehören
findRules :: Symbol -> [Rule] -> [Production]
findRules _ []     = []
findRules sym (r:rs)
    | lhs r == sym = rhs r ++ findRules sym rs  --Falls die linke Seite der Regel gleich sym ist → alle Produktionen dieser Regel nehmen und rekursiv weitersuchen
    | otherwise    = findRules sym rs            -- sonst regel überspringen und weitersuchen

-- Aufgabe 7 berechnet First Menge  firstSetProd testGrammar [T "+", N "T", N "E'"]
firstSetProd :: Grammar -> Production -> [Symbol]
firstSetProd _ []          = [Epsilon]
firstSetProd _ (Epsilon:_) = [Epsilon] -- Produktion beginnt mit Epsilion oder T -> First gleich Epsilion oder T
firstSetProd _ (T t : _)   = [T t]
firstSetProd (Grammar rules) (N n : rest) =
    let prods   = findRules (N n) rules -- Suche alle Produktionen von N n mit findRules
        firstN  = firstSetHelper (Grammar rules) prods -- Berechne FIRST aller Produktionen von N n
        ohneEps = removeEpsilon firstN -- Entferne Epsilon aus dem Ergebnis
    in if containsEpsilon firstN    -- Prüfe ob Epsilon in FIRST(N n) enthalten ist

       then ohneEps ++ firstSetProd (Grammar rules) rest -- Falls ja → N n kann verschwinden, also muss auch FIRST(rest) hinzugefügt werden
       else ohneEps 

-- Aufgabe 8    -- firstSetHelper testGrammar [[T "+", N "T", N "E'"], [Epsilon]]

firstSetHelper :: Grammar -> [Production] -> [Symbol]   -- SINN: Wendet firstSetProd auf eine ganze Liste von Produktionen an fasst alle Ergebnisse zusammen
firstSetHelper _ []     = []
firstSetHelper g (p:ps) = nub (firstSetProd g p ++ firstSetHelper g ps)

-- Aufgabe 9    --berechnet die komplette FIRST-Menge
firstSet :: Grammar -> Symbol -> [Symbol]   -- firstSet testGrammar (N "E'")

firstSet g@(Grammar rules) sym =        -- g die ganze Grammatik (wird an firstSetHelper weitergegeben)
                                        -- rules nur die Regeln (wird an findRules weitergegeben)
                                        -- @ erlaubt beides gleichzeitig zu benutzen
    let prods = findRules sym rules     --Suche alle Produktionen die zu sym gehören
    in nub (firstSetHelper g prods)     --Berechne FIRST aller gefundenen Produktionen


-- Testgrammatik G
testGrammar :: Grammar
testGrammar = Grammar
    [ Rule { lhs = N "E",  rhs = [[N "T", N "E'"]] }
    , Rule { lhs = N "E'", rhs = [[T "+", N "T", N "E'"], [Epsilon]] }
    , Rule { lhs = N "T",  rhs = [[N "F", N "T'"]] }
    , Rule { lhs = N "T'", rhs = [[T "*", N "F", N "T'"], [Epsilon]] }
    , Rule { lhs = N "F",  rhs = [[T "(", N "E", T ")"], [T "id"]] }
    ]