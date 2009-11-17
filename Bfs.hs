module Bfs where

import Tree

bfs :: (Eq a) => Tree a -> [Tree a]
bfs t = bfs' [t] [] 
bfs' open closed = case (reverse open) of
    [] -> closed
    (Goal:_) -> Goal:closed
    (x:xs) -> let uniqueChildren = generateUniqueChildren x (open ++ closed)
              in bfs' (uniqueChildren ++ (reverse xs)) (x:closed)
