import Text.Printf
import Control.Monad
import Data.List
import Data.Function
import Data.Ord

lowest :: [(Int, Int)] -> (Int, Int)
lowest [b] = b
lowest (b1:b2:bs)
        |snd b1 < snd b2 = lowest (b1:bs)
        |snd b1 > snd b2 = lowest (b2:bs)
        |otherwise     = 
            if fst b1 > fst b2
                then lowest (b2:bs)
                else
                    lowest (b1:bs)

--compute tan of angle
angle :: (Floating f, Integral i) => (i, i) -> (i, i) -> f
angle a b
    |a == b         = -1
    |fst a > fst b  = abs(angle) + pi/2
    |otherwise      = atan (fromIntegral (snd b - snd a) / fromIntegral (fst b - fst a)) 
    where angle = atan (fromIntegral (fst b - fst a) / fromIntegral (snd b - snd a))

--sort by angle with x-axis (vector formed by lowest and other points)
sortAngle :: (Integral i) => (i, i) -> [(i, i)] -> [(i, i)]
sortAngle low lst = (sortBy (compare `on` (angle low))) (sortBy (compare `on` snd) lst)

--iterate and check if left or right turn
convexHull' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
convexHull' ch [] = ch
convexHull' ch [n] = n:ch
convexHull' (p:xs) (c:n:ys) =
	if ((fst c - fst p)*(snd n - snd p)-(snd c - snd p)*(fst n - fst p)) > 0
		then convexHull' (c:p:xs) (n:ys)
		else
			convexHull' (p:xs) (n:ys)

convexHull :: [(Int,Int)] -> [(Int, Int)]
convexHull [] = []
convexHull (x:xs)
    |(x:xs) == ch = ch
	|otherwise            = convexHull ch
	where ch = reverse (convexHull' [x] xs)
                    
removeSame :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
removeSame rm [] _ = reverse rm
removeSame [] (y:ys) low = removeSame [y] ys low
removeSame (x:xs) (y:ys) low =
    |angle low x == angle low y = removeSame (y:xs) ys low
    |otherwise 	                = removeSame (y:x:xs) ys low

perimeter :: (Int, Int) -> [(Int, Int)] -> Double
perimeter _ []     = 0
perimeter a (x:xs) = magnitude a x + perimeter x xs

magnitude :: (Int, Int) -> (Int, Int) -> Double
magnitude (a, b) (c, d) = sqrt $ fromIntegral ((a-c)^2+(b-d)^2)

readPair :: IO (Int, Int)
readPair = (\[x, y] -> (x, y)) . map read . words <$> getLine

main :: IO ()
main = do
  n <- readLn :: IO Int
  points <- replicateM n readPair
  let
    sorted_points = removeSame [] (sortAngle (lowest points) points) (lowest points)
    convex_hull = convexHull sorted_points
    perim = perimeter (head convex_hull) ((tail convex_hull) ++ [head convex_hull])
  printf "%.1f\n" perim