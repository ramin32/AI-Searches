{---------------------------------------------
 - Ramin Rakhamimov
 - CIS 716
 - BFS search module.
 ---------------------------------------------}
 
module BranchAndBound where

import Data.List 
import Tree

branchAndBound :: (Ord a, Eq a) => Tree a -> [Tree a]
branchAndBound t = branchAndBound' [t] [] 
branchAndBound' open closed = case open of
    [] -> closed
    (Goal:_) -> Goal:closed
    (x:xs) -> let uniqueChildren = generateUniqueChildren x (open ++ closed)
                  weightedChildren = map (\y -> addWeight y (weight x)) uniqueChildren
                  prioritizedOpenList = (foldl (\acc y -> insertBy compareWeight y acc)
                                               xs
                                               weightedChildren)
                  in branchAndBound' prioritizedOpenList (x:closed)
