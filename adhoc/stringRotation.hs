import Control.Monad

rotate :: String -> String
rotate (x:xs) = xs ++ [x]

possible :: Int -> String -> String
possible 0 _ = ""
possible (-1) x = possible (length x) x
possible n x = rot ++ " " ++ possible (n-1) rot
    where rot = rotate x

pretty :: [String] -> String
pretty [] = ""
pretty (x:xs) = x ++ "\n" ++ pretty xs

main :: IO ()
main = do
    count <- getLine
    let lines = read count :: Int
    x <- replicateM lines getLine
    let rotated = pretty (map (possible (-1)) x)
    putStr rotated

--let strings = readInput
