data Occurrence a = Occurrence a a a a deriving (Show)

--fails if char not RGBY
addOcc :: (Integral a) => Occurrence a -> Char -> Occurrence a
addOcc (Occurrence r g b y) ch = case ch of
                                    'R' -> Occurrence (r+1) g b y
                                    'G' -> Occurrence r (g+1) b y
                                    'B' -> Occurrence r g (b+1) y
                                    'Y' -> Occurrence r g b (y+1)
                                    
prefix :: (Integral a) => Occurrence a -> Bool
prefix (Occurrence r g b y)
            | difference r g > 1 = False
            | difference b y > 1 = False
            | otherwise          = True
            
fullOfColors :: (Integral a) => Occurrence a -> Bool
fullOfColors (Occurrence r g b y)
            | difference r g /= 0 = False
            | difference b y /= 0 = False
            | otherwise           = True

repl :: String -> String
repl input =
        let sequences = tail (lines input)
            result = map (checkColors (Occurrence 0 0 0 0)) sequences
        in unlines (map show result)
        
checkColors :: Occurrence Int -> String -> Bool
checkColors occ [] =
            if fullOfColors occ
                then True
            else
                False
checkColors occ (x:xs) = 
            if prefix occ'
                then checkColors occ' xs
            else
               False
            where occ' = addOcc occ x
               
difference :: (Integral a) => a -> a -> a
difference a b = abs (a-b)

main :: IO ()
main = interact repl
