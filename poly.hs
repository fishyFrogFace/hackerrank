import System.IO
import Control.Applicative
import Control.Monad

data Vector = Vector { x :: Int
                     , y :: Int
                     } deriving (Show)

readInts :: IO [Int]
readInts = fmap (map read.words) getLine

addend :: [[Int]] -> [[Int]]
addend [] = []
addend (x:xs) = x : xs ++ [x]

magnitude :: (Floating a) => Vector -> Vector -> a
magnitude (Vector a b) (Vector x y) = sqrt . fromIntegral $ ((a-x)^2 + (b-y)^2)

toVector :: [[Int]] -> [Vector]
toVector [] = []
toVector (x:xs) = (Vector (head x) (head $ tail x)) : toVector xs

calculate :: (Floating a) => Vector -> [Vector] -> a
calculate _ [] = 0
calculate a (x:xs) = magnitude a x + (calculate x xs)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    x <- replicateM n readInts
    let vectors = toVector $ addend x
    print (calculate (head vectors) (tail vectors))
