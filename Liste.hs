k :: [Int] -> Int 
k[] = 0 --falls Liste leer
k (x:xs) = x + k xs



mapList :: (Int -> Int) -> [Int] -> [Int]
mapList f []     = []                  -- Basisfall: leere Liste
mapList f (x:xs) = f x : mapList f xs -- f auf x anwenden, Rest rekursiv

quadratic :: (Int, Int, Int) -> Int -> Int
quadratic (a, b, c) x = a*x^2+b*x+c


square :: Int -> Int            --ist nicht ganz richig, da ich das Summenzeichen bissl ignoriert habe whoopsie
square x 
    | x < 0 =  -(-x^2)
    | x == 0 = 0
    | otherwise = x^2