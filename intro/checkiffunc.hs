import Control.Applicative
import Control.Monad
import System.IO
import Data.List

readInts :: IO [Int]
readInts = fmap (map read.words) getLine

relation :: [Int] -> [[Int]] -> String
relation [] (x:xs) = relation x xs
relation _ []      = "YES"
relation (x:xs) ((n:ns):ys)
        | x == n && xs /= ns = "NO"
        | otherwise          = relation (n:ns) ys

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        m_temp <- getLine
        let m = read m_temp :: Int
        x <-  replicateM m readInts
        putStrLn (relation [] (sort x))

        --forM_ [1..m] $ \a0  -> do
           --x_temp <- readInts
          -- let x = read x_temp :: [Int]
