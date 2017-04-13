avgtime :: (Eq t, Fractional t) => t -> t -> t
avgtime _ 0 = 0
avgtime bubbles n = bubbles/n + avgtime bubbles (n-1)

main = do
    x <- fmap (map read . words) getLine
    let bubbles = realToFrac (head x * head (tail x))
    print (avgtime bubbles bubbles)
