import Control.Lens
import Control.Monad
import Data.Sequence
import Prelude hiding (lookup)

vals = fromList [1,1]

newVal :: Int -> Seq Int -> (Seq Int, Int)
newVal n acc = case (lookup n acc) of
                Just x -> (acc, x)
                Nothing -> (acc Data.Sequence.|> new, new)
                    where
                        new = trees' n 1 acc `mod` 100000007

buildAcc :: Int -> Seq Int -> (Seq Int, Int)
buildAcc n acc = build' n (Data.Sequence.length acc) acc
                    where
                        build' :: Int -> Int -> Seq Int -> (Seq Int, Int)
                        build' n i acc
                            |n < i     = (acc, trees n acc)
                            |otherwise = build' n (i+1) (fst $ newVal i acc)

trees :: Int -> Seq Int -> Int
trees n acc = case (lookup n acc) of
                Just x -> x
                Nothing -> trees' n 1 acc

trees' :: Int -> Int -> Seq Int -> Int
trees' n i acc
    |n < i     = 0
    |otherwise = nth + (trees' n (i+1) acc)
        where 
            nth = (trees (i-1) acc) * (trees (n-i) acc)

loop :: [Int] -> Seq Int -> [Int]
loop [] acc = []
loop (x:xs) acc = (snd result) : loop xs (fst result)
                    where
                        result = buildAcc x acc

main :: IO ()
main = do
    cases <- readLn :: IO Int
    nodes <- Control.Monad.replicateM cases (readLn :: IO Int)
    mapM_ print (loop nodes vals)
