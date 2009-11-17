module AStar where

import Data.List 
import Tree

aStar :: (Ord a, Eq a) => Tree a -> [Tree a]
aStar t = aStar' [t] [] 
aStar' open closed = case open of
    [] -> closed
    (Goal:_) -> Goal:closed
    (x:xs) -> let uniqueChildren = generateUniqueChildren x (open ++ closed)
                  weightedChildren = map (\y -> addWeight y (weight x)) uniqueChildren
                  heuristifiedChildren = map (\y -> addHeuristic y (weight y)) weightedChildren
                  prioritizedOpenList = (foldl (\acc y -> insertBy compareHeuristic y acc)
                                               xs
                                               heuristifiedChildren)
                  in aStar' prioritizedOpenList (x:closed)
