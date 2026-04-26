k :: [Int] -> Int 
k[] = 0 --falls Liste leer
k (x:xs) = x + k xs
