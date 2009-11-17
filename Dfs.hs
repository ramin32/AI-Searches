module Dfs where

import Tree

dfs :: (Eq a) => Tree a -> [Tree a]
dfs t = dfs' [t] [] 
dfs' open closed = case open of
    [] -> closed
    (Goal:_) -> Goal:closed
    (x:xs) -> let uniqueChildren = generateUniqueChildren x (open ++ closed)
              in dfs' (uniqueChildren ++ xs) (x:closed)
