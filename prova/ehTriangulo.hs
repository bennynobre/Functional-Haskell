ehTriangulo :: Int -> Int -> Int -> Bool
ehTriangulo a b c 
    | a + b > c && a + c > b && b + c > a = True
    | otherwise = False

main :: IO ()
main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    c <- readLn :: IO Int
    print $ ehTriangulo a b c