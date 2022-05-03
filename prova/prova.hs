ehTriangulo :: Int -> Int -> Int -> Bool
ehTriangulo a b c 
    | a + b > c && a + c > b && b + c > a = True
    | otherwise = False

ehPoligono :: [Int] -> Bool
ehPoligono xs = length [x | x <- xs, x >= sum xs - x] == 0

index_of :: Int -> [Int] -> Int
index_of x xs = if (length xs) == ys then -1 else ys
    where ys = index x xs


index x ys
   | ys == [] = -1
   | x == head ys = 0
   | x > length ys = -1
   | otherwise = 1 + (index x (tail ys))


-- Procure o indice de um elemento x em uma lista xs usando foldl
index_of_fold :: Int -> [Int] -> Int
index_of_fold x xs = foldl (\acc x -> if x == xs then acc else acc + 1) 0 xs

{-
index_of :: Int -> [Int] -> Int
index_of x(y:ys) =
    if x == y
        then 0
    else if ys == []
        then -1
    else if x > length ys
        then -1
    else 1 + index_of x ys
-}
