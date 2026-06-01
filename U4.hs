import Data.Maybe

-- Teil 1: id_list Parser
match :: String -> [String] -> [String]
match expected [] = error ("Erwartet: " ++ expected ++ " aber Eingabe leer!")
match expected (x:xs)
    | expected == x = xs
    | otherwise     = error ("Erwartet: " ++ expected ++ " aber gefunden: " ++ x)

id_list_tail :: [String] -> [String]
id_list_tail [] = error "Unerwartetes Ende der Eingabe!"
id_list_tail (x:xs)
    | x == ","  = id_list_tail (match "id" xs)
    | x == ";"  = match "$$" xs
    | otherwise = error ("Unerwartetes Token: " ++ x)

id_list :: [String] -> [String]
id_list [] = error "Eingabe leer!"
id_list xs = id_list_tail (match "id" xs)


-- Teil 2: Ausdruck Parser mit Maybe Code prüft, ob Mathematische "Grammatik" eingehalten wird 
{-prog   → expr '$'
expr   → term ttail
term   → factor ftail
ttail  → '+' term ttail | ε
factor → 'c'
ftail  → '*' factor ftail | ε-}

matchM :: Char -> Maybe String -> Maybe String
matchM _ Nothing = Nothing  --fehler weitergeben
matchM c (Just []) = Nothing    --leere eingabe 
matchM c (Just (x:xs))          
    | c == x    = Just xs   --falls token passt wird rest zurückgegeben
    | otherwise = Nothing

factor :: Maybe String -> Maybe String  --erkennt zeichen "c"
factor = matchM 'c'

ftail :: Maybe String -> Maybe String   --erkennt "*"
ftail Nothing = Nothing
ftail (Just []) = Just []
ftail (Just (x:xs))
    | x == '*'  = ftail (factor (Just xs))  --falls "*" gefunden dann ftail, sonst passiert nichts
    | otherwise = Just (x:xs)

term :: Maybe String -> Maybe String
term = ftail . factor   --verbindet factor mit ftail

ttail :: Maybe String -> Maybe String   --erkennt +
ttail Nothing = Nothing
ttail (Just []) = Just []
ttail (Just (x:xs))
    | x == '+'  = ttail (term (Just xs))
    | otherwise = Just (x:xs)

expr :: Maybe String -> Maybe String
expr = ttail . term --verbinden

prog :: String -> Maybe String  --erkennt den gesamten ausdruck
prog s = matchM '$' (expr (Just s))     

{-bsp prog "c+c*c$"   =  Just ""   -- gültiger 
prog "c*c+c$"   =  Just ""   -- gültiger  
prog "c$"       =  Just ""   -- gültiger  
prog "c+c-c$"   =  Nothing   -- '-'  nicht erlaubt 
prog "cc$"      =  Nothing   -- zwei c hintereinander 
prog "c+$"      =  Nothing   -- '+' ohne rechte Seite -}