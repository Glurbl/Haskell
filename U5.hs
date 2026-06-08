-- Aufgabe 1: Sortieren

splitByKey :: Int -> [Int] -> ([Int], [Int])
splitByKey _ [] = ([], [])
splitByKey key (x:xs)
    | x <= key  = (x : kleiner, groesser)
    | otherwise = (kleiner, x : groesser)
    where (kleiner, groesser) = splitByKey key xs

sortQuick :: [Int] -> [Int]
sortQuick [] = []
sortQuick (x:xs) = sortQuick kleiner ++ [x] ++ sortQuick groesser
    where (kleiner, groesser) = splitByKey x xs

splitInHalf :: [Int] -> ([Int], [Int])
splitInHalf xs = (take half xs, drop half xs)
    where half = length xs `div` 2

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x <= y    = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

sortMerge :: [Int] -> [Int]
sortMerge [] = []
sortMerge [x] = [x]
sortMerge xs = mergeLists (sortMerge links) (sortMerge rechts)
    where (links, rechts) = splitInHalf xs


-- Aufgabe 2: Addierer

type Nibble = (Bool, Bool, Bool, Bool)

bitToInt :: Bool -> Int     --wandelt einzelne Bit in Zahlen um
bitToInt False = 0
bitToInt True  = 1

dualValue :: Nibble -> Int      --Bit hat stellenwert von rechts nach links 
dualValue (b3, b2, b1, b0) =   bitToInt b0 * 1      --verdoppelt sich jedes Mal 1001 = 1+0+0+8 = 9
                              + bitToInt b1 * 2
                              + bitToInt b2 * 4
                              + bitToInt b3 * 8

zweiKomplement :: Nibble -> Int     --b3 wird negiert 
zweiKomplement (b3, b2, b1, b0) =   (- bitToInt b3 * 8)
                                   + bitToInt b2 * 4
                                   + bitToInt b1 * 2
                                   + bitToInt b0 * 1

bitString :: Nibble -> String   --wandelt jedes Bit in 0 oder 1 und fügt Sie zusammen
bitString (b3, b2, b1, b0) =   show (bitToInt b3)
                             ++ show (bitToInt b2)
                             ++ show (bitToInt b1)
                             ++ show (bitToInt b0)

showNibble :: Nibble -> String  --hängt alles zusammen
showNibble n =  bitString n
             ++ " "
             ++ show (dualValue n)
             ++ " "
             ++ show (zweiKomplement n)

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)  --gibt True zurück wenn genau eins True ist 

bitAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
bitAdder x y z = (c, s)
    where
        s'  = xor x y
        c'  = x && y
        s   = xor s' z
        c'' = s' && z
        c   = c' || c''

{-x=True, y=True, z=True

Schritt 1: s'  = xor True True   = False
Schritt 2: c'  = True && True    = True
Schritt 3: s   = xor False True  = True
Schritt 4: c'' = False && True   = False
Schritt 5: c   = True || False   = True

Ergebnis: (True, True) = "11" in Binär = 3 in Dezimal 
 1+1+1 = 3  -}        

nibbleAdder :: Nibble -> Nibble -> (Bool, Nibble)   --Bitadder werden hintereinander ausgeführt, der übertrage wird jedes Mal ans nächste weitergeleitet
nibbleAdder (x3,x2,x1,x0) (y3,y2,y1,y0) = (c, (s3,s2,s1,s0))
    where
        (c1, s0) = bitAdder x0 y0 False
        (c2, s1) = bitAdder x1 y1 c1
        (c3, s2) = bitAdder x2 y2 c2
        (c,  s3) = bitAdder x3 y3 c3

zeileBauen :: (Nibble -> Nibble -> (Bool, Nibble)) -> (Nibble, Nibble) -> String    --baut die Tabelle
zeileBauen f (n1, n2) =   showNibble n1
                        ++ " + "
                        ++ showNibble n2
                        ++ " = "
                        ++ show ueberlauf
                        ++ " "
                        ++ showNibble ergebnis
    where (ueberlauf, ergebnis) = f n1 n2

tableAdder :: (Nibble -> Nibble -> (Bool, Nibble)) -> [(Nibble, Nibble)] -> String
tableAdder f paare = unlines (map (zeileBauen f) paare)