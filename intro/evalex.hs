import Control.Applicative
import Control.Monad
import System.IO

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

ex :: Int -> Double -> Double
ex 0 _ = 1
ex n x = x^n/fromIntegral (factorial n) + ex (n-1) x

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        x_temp <- getLine
        let x = read x_temp :: Double
        putStrLn (show (ex 9 x))
