{---------------------------------------------
 - Ramin Rakhamimov
 - CIS 716
 - A* search module.
 ---------------------------------------------}
 
module AStar where

import Data.List 
import Tree

aStar :: (Ord a, Eq a) => Tree a -> [Tree a]
aStar t = aStar' [t] [] 
aStar' open closed = case open of
    [] -> closed
    (Goal:_) -> Goal:closed
    (x:xs) -> let children = generateChildren x 
                  weightedChildren = map (\y -> addWeight y (weight x)) children
                  heuristifiedChildren = map (\y -> addHeuristic y (weight y)) weightedChildren
                  prioritizedOpenList = (foldl (\acc y -> insertBy compareHeuristic y acc)
                                               xs
                                               heuristifiedChildren)
                  -- Dyanmic programming step
                  cleanList = nub prioritizedOpenList -- Removes duplicates (ie: worse nodes) 
                  in aStar' cleanList (x:closed)
