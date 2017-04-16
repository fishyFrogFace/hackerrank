import Data.List

getDigits :: Integer -> [Integer]
getDigits 0 = []
getDigits n = n `mod` 10 : getDigits (n `div` 10)

replInteger :: [Integer] -> Integer
replInteger [int, count]
        |count == 0 = 0
        |otherwise  = count * sum (getDigits int)
        
superDigit :: Integer -> Integer
superDigit n
        |length s == 1 = sum s
        |otherwise     = superDigit (sum s)
            where s = getDigits n

main :: IO ()
main = do
    inputs <- getLine
    let lst = map read $ words inputs :: [Integer]
    print $ superDigit (replInteger lst)
