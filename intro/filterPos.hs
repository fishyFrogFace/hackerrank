f :: Int -> [Int] -> [Int]
f n [] = []
f n (x:xs)
    | n == 0    =   x : f 1 xs
    | n == 1    =   f 0 xs

b = f 0

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). b. map read. lines $ inputdata
