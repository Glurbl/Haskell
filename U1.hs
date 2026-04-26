
foo :: Int -> [Int] -> [Int]
foo k [] = []
foo k (x : xs)
  | k /= x    = x : foo k xs 
  | otherwise = xs



quadratic :: (Int, Int, Int) -> Int -> Int
quadratic (a, b, c) x = a*x^2+b*x+c


square :: Int -> Int            --ist nicht ganz richig, da ich das Summenzeichen bissl ignoriert habe whoopsie
square x 
    | x < 0 =  -(-x^2)
    | x == 0 = 0
    | otherwise = x^2


{-sumList :: [Int] -> Int
sumList  [] = []
sumList  [] !! 0 = x
y= y + x-}

k :: [Int] -> Int 
k[] = 0     --falls Liste leer
k (x:xs) = x + k xs   {-k [1, 2, 3]
                        = 1 + s [2, 3]
                        = 1 + 2 + s [3]
                        = 1 + 2 + 3 + s []
                        = 1 + 2 + 3 + 0
                        = 6 -}


tableInt :: (Int -> Int) -> [Int] -> String
tableInt f liste =  unlines (map (\x -> show x ++ ":" ++ show (f x)) liste)    --(\x -> show x ++ ":" ++ show (f x)) liste)    
                                                                              -- Lambda funktion: Funktion welche nur "kurz" benötigt wird und keinen eigenen namen benötigt
                                                                              --show konvertiert Int zu String hier: -10 : (f x -10) -> -10:100
                                                                              --map wendet Lambda funktion auf jedes Element der Liste an 
                                                                              -- unlines gibt jedes Mal einzelnen String zurück, ohne wäre es eine LISTE an Strings -> [String]