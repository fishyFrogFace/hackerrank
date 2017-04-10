remove :: String -> String -> String
remove a [] = a
remove a (x:xs)
    |x `elem` a = remove a xs
    |otherwise = remove (a ++ [x]) xs

main :: IO ()
main = do
    x <- getLine
    putStrLn (remove "" x)    
