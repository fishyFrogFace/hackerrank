mport System.IO
import Control.Applicative
import Control.Monad

data Vector = Vector { x :: Int
                     , y :: Int
                     } deriving (Show)

addend :: [a] -> [a]
addend [] = []
addend (x:xs) = x : xs ++ [x]

toVector :: [Int] -> Vector
toVector (x:xs) = Vector x (head xs)

calculate :: (Floating a) => Vector -> [Vector] -> a
calculate _ [] = 0
calculate vec1 (vec2:xs) = (fromIntegral (x vec1*y vec2) - fromIntegral (y vec1*x vec2))/2 + calculate vec2 xs

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    x <- replicateM n (fmap (map read.words) getLine)
    let vectors = addend (map toVector x)
    print (calculate (head vectors) (tail vectors))
