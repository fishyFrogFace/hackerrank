lcd :: (Integral a) => a -> a -> a
lcd x y = (abs x*y) `quot` (gcd x y)

calculate :: (Integral a) => [a] -> a
calculate [] = 1
calculate (x:xs) = lcd x (calculate xs)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    x <- fmap (map read.words) getLine
    print . calculate $ x
