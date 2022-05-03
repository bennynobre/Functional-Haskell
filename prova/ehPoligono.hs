eh_Poligono :: [Int] -> Bool
eh_Poligono ys = length [x | x <- ys, x >= sum ys - x] == 0

main :: IO ()
main = do
    ys <- readLn :: IO [Int]
    print $ eh_Poligono ys