import U1
import Data.Char (toLower)


-- Aufgabe 1 (Berechnet Ergebnis aller Werte einer Liste wobei Operator in Kommandozeile mitgegeben wird, zB (+) oder max)
foldList :: (Double -> Double -> Double) -> [Double] -> Double -- Typsignatur (foldList bekommt eine Funktion bei der aus zwei Double ein Double wird, eine Liste von Double-Werten, und liefert einen Double zurück)
foldList f [x] = x -- Basisfall für wenn nur noch ein Element in Liste übrig
foldList f (x:xs) = f x (foldList f xs) -- x wird rekurisv mit xs kombiniert 



-- Aufgabe 2 (Wendet auf jeden Wert einer Liste eine bestimmte Funktion an und gibt jedes Ergebnis in einer neuen Liste zurück)
mapList :: (Int -> Int) -> [Int] -> [Int] -- Typsignatur (mapList bekommt ne Funktion und eine Liste von Ints gegeben und gibt eine Liste von ints zurück)
mapList f [] = []       -- Basisfall wenn Liste leer
mapList f (x:xs) = f x : mapList f xs -- Erster Wert der Liste wird rekursiv aufgerufen und an Ergebnis Liste angehängt // Eingabe: square x = x*x // mapList square [1..10]

{-quadratic :: (Int, Int, Int) -> Int -> Int
quadratic (a,b,c) x = a*x*x + b*x + c-}



-- Aufgabe 3 (Prüft ob ein Int in einer Liste von Ints vorkommt und gibt Bool zurück)
containsList :: [Int] -> Int -> Bool -- Typsignatur (Liste von Ints und ein anderer Int werden gegeben und es wird ein Bool zurückgegeben)
containsList [] a = False
containsList (x:xs) a = 
    if (x==a)
      then True
      else containsList xs a

{-containsList :: [Int] -> Int -> Bool
containsList [] n = False
containsList (x:xs) n
    | x == n    = True
    | otherwise = containsList xs n -}



-- Aufgabe 4
 --Import: Lädt Funktion toLower, mit der zu Großbuchsteiben der Kleinbuchstabe zurückgeliefert wird
countList :: [Char] -> Char -> Int -- Typsignatur (Liste mit Char wird übergeben und ein Char, Ausgegeben wird Anzahl der Char in der Liste (Eingabe: countList "Hallo" 'l' -> sollte zwei ergeben))
countList [] c = 0 -- Basisfall wenn Liste leer
countList (x:xs) c = 
    if toLower x == toLower c -- wenn Listenwert x == Argument c
      then 1 + countList xs c -- dann 1 + ergebnis der Rekursion von countList mit xs
      else countList xs c -- sonst Rekursion von countList mit xs aber ohne 1 zu addieren