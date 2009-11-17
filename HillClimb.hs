module HillClimb where

import Data.List 
import Tree

hillClimb :: (Ord a, Eq a) => Tree a -> [Tree a]
hillClimb t = hillClimb' [t] [] 
hillClimb' open closed = case open of
    [] -> closed
    (Goal:_) -> Goal:closed
    (x:xs) -> let uniqueChildren = generateUniqueChildren x (open ++ closed)
                  prioritizedOpenList = (foldl (\acc x -> insertBy compareHeuristic x acc)
                                               xs
                                               uniqueChildren)
                  in hillClimb' prioritizedOpenList (x:closed)
