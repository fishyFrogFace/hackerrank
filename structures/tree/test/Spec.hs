import Test.Hspec
import Test.QuickCheck
import Data.List.Split

import Lib

--define a tree here
completeLst = ((take 3 (chunksOf 2 [2..])) ++ take 4 (repeat [-1, -1]))
completeTree = Branch 1 
                      (Branch 2 (Branch 4 Empty Empty) (Branch 5 Empty Empty))
                      (Branch 3 (Branch 6 Empty Empty) (Branch 7 Empty Empty))
completeSwap = Branch 1
                      (Branch 3 (Branch 6 Empty Empty) (Branch 7 Empty Empty))
                      (Branch 2 (Branch 4 Empty Empty) (Branch 5 Empty Empty))
incompleteOrd = [[2,3],[4,5],[6,-1],[7,-1],[-1,-1],[-1,8],[-1,-1], [-1,-1]]
incompleteOrdT = Branch 1 
                        (Branch 2 (Branch 4 (Branch 7 Empty Empty) Empty) (Branch 5 Empty Empty)) 
                        (Branch 3 (Branch 6 Empty (Branch 8 Empty Empty)) Empty)
incompleteOrdS = Branch 1 
                        (Branch 2 (Branch 5 Empty Empty) (Branch 4 (Branch 7 Empty Empty) Empty)) 
                        (Branch 3 Empty (Branch 6 Empty (Branch 8 Empty Empty)))
incomplete = [[3,2],[6,4],[5,-1],[7,-1],[-1,-1],[-1,8],[-1,-1], [-1,-1]]
incompleteT = Branch 1 
                     (Branch 3 (Branch 5 Empty Empty) Empty) 
                     (Branch 2 (Branch 6 Empty (Branch 8 Empty Empty)) (Branch 4 (Branch 7 Empty Empty) Empty))


main :: IO ()
main = hspec $ do
    describe "fill" $ do
        it "can fill a complete binary tree with ordered nodes" $ do
            fill 1 completeLst `shouldBe` completeTree
        it "can fill an incomplete binary tree with ordered nodes" $ do
            fill 1 incompleteOrd `shouldBe` incompleteOrdT
        it "can fill an incomplete binary tree without ordered nodes" $ do
            fill 1 incomplete `shouldBe` incompleteT
    describe "traverseTree" $ do
        it "can traverse a complete binary tree with ordered nodes" $ do
            traverseTree completeTree `shouldBe` "2 4 5 1 3 6 7 "
        it "can traverse an incomplete binary tree with ordered nodes" $ do
            traverseTree incompleteOrdT `shouldBe` "2 4 7 5 1 3 6 8 "
        it "can traverse an incomplete binary tree without ordered nodes" $ do
            traverseTree incompleteT `shouldBe` "3 5 1 2 6 8 4 7 "
    describe "swap" $ do
        it "can swap a complete binary tree with ordered nodes" $ do
            swap completeTree 2 `shouldBe` completeSwap
        it "can swap an incomplete binary tree with ordered nodes" $ do
            swap incompleteOrdT 3 `shouldBe` incompleteOrdS


