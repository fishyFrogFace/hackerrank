pascal :: (Integral a) => a -> [a] -> [a]
pascal _ [] = []
pascal n (x:xs) = (n+x) : (pascal x xs)

pascal' :: (Integral a) => a -> [a] -> [a]
pascal' 1 x = x
pascal' n x = pascal' (n-1) (pascal 0 (x ++ [0]))

makeString :: [String] -> String
makeString [x] = x
makeString (x:xs) = x ++ ' ' : makeString xs

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    let pascals = [pascal' i [1] | i <- [1..n]]
    let strings = map makeString (map (map show) pascals)
    mapM_ putStrLn strings
