import Data.List

findmissing :: [Int] -> [Int] -> Int -> [Int]
findmissing _ _ 0 = []
findmissing [] y _ = y
findmissing (x:xs) (y:ys) n
        |x == y    = findmissing xs ys n
        |otherwise = y : findmissing (x:xs) ys (n-1)
        
removeDuplicates :: (Eq a) => [a] -> [a] -> [a]
removeDuplicates x [] = x
removeDuplicates x (y:ys)
        |y `elem` x = removeDuplicates x ys
        |otherwise  = removeDuplicates (x ++ [y]) ys
        
tostring :: [String]-> String
tostring [] = ""
tostring (x:xs) = x ++ " " ++ tostring xs

readInts :: IO [Int]
readInts = fmap (map read . words) getLine 

main :: IO ()
main = do
    lenA_ <- getLine
    let lenA = read lenA_ :: Int
    listA <- readInts
    lenB_ <- getLine
    let lenB = read lenB_ :: Int
    listB <- readInts
    let difference = lenB-lenA
    let missing = removeDuplicates [] (findmissing (sort listA) (sort listB) (lenB-lenA))
    let output = tostring (map show missing)
    putStrLn output

