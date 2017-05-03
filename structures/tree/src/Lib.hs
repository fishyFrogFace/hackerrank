module Lib
    ( Tree(..)
    , traverseTree
    , fill
    , swap
    ) where 

import Control.Monad

data Tree a = Branch a (Tree a) (Tree a)
              | Empty
              deriving (Show, Eq)

traverseTree :: (Show a) => Tree a -> String
traverseTree Empty = ""
traverseTree (Branch n l r) = traverseTree l ++ (show n) ++ " " ++ traverseTree r

fill :: Int -> [[Int]] -> Tree Int
fill parent lst =
    if parent == -1
        then Empty
    else
        Branch parent (fill (newPar !! 0) lst) (fill (newPar !! 1) lst)
        where newPar = lst !! (parent-1)

swap :: Tree a -> Int -> Tree a
swap Empty _ = Empty
swap (Branch n l r) depth =
    if depth == 1 
        then Branch n r l
    else
        Branch n (swap l (depth-1)) (swap r (depth-1))

maxDepth :: Tree a -> Int -> Int
maxDepth Empty n = n
maxDepth (Branch _ l r) n = max (maxDepth l (n+1)) (maxDepth r (n+1))

swapQueue :: [Int] -> Int -> [[Int]]
swapQueue [] _ = []
swapQueue (x:xs) depth = (nrQ x depth 1) : (swapQueue xs depth)
    where
        nrQ :: Int -> Int -> Int -> [Int]
        nrQ n depth iter =
            if nextMult > depth
                then []
            else
            nextMult : (nrQ n depth (iter+1))
            where nextMult = n*iter

performSwaps :: [Int] -> Tree a -> Tree a
performSwaps [] tree = tree
performSwaps (x:xs) tree = performSwaps xs (swap tree x)

swapSteps :: Tree a -> [[Int]] -> [Tree a]
swapSteps tree [] = []
swapSteps tree (x:xs) = new : swapSteps new xs
    where new = performSwaps x tree

readInts :: IO [Int]
readInts = fmap (map read . words) getLine

main :: IO ()
main = do
    nodeNr <- readLn :: IO Int
    nodes <- replicateM nodeNr readInts
    swapNr <- readLn :: IO Int
    swapLst <- replicateM swapNr (readLn :: IO Int)
    let tree = fill 1 nodes
        depth = maxDepth tree 0
        swapResults = swapSteps tree (swapQueue swapLst depth)
    mapM_ putStrLn (map traverseTree swapResults)
