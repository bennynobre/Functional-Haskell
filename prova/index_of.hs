index_of :: Int -> [Int] -> Int
index_of x xs = if (length xs) == ys then -1 else ys
    where ys = index x xs


index x ys
   | ys == [] = -1
   | x == head ys = 0
   | x > length ys = -1
   | x == length ys = x - 1
   | otherwise = 1 + (index x (tail ys))

main :: IO ()
main = do
    xs <- readLn :: IO [Int]
    y <- readLn :: IO Int
    print $ index_of y xs