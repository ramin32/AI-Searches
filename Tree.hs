{---------------------------------------------
 - Ramin Rakhamimov
 - CIS 716
 - ADT for Tree type.
 ---------------------------------------------}
 
module Tree where

type Weight = Int 
type Heuristic = Int 
data Tree a = Void | Goal | Node a Weight Heuristic (Tree a) (Tree a) deriving (Ord) 

instance (Eq a) => Eq (Tree a) where
    (Node x _ _ _ _) == (Node y _ _ _ _) = x == y
    Void == Void = True
    Goal == Goal = True
    _ == _ = False

instance (Show a) => Show (Tree a) where
    show Void = []
    show Goal = "Goal"
    show (Node a _ _ _ _) = show a 


compareHeuristic :: (Ord a) => Tree a -> Tree a -> Ordering
compareHeuristic (Node _ _ h1 _ _) (Node _ _ h2 _ _) = compare h1 h2 
compareHeuristic t t1 = compare t t1

compareWeight :: (Ord a) => Tree a -> Tree a -> Ordering
compareWeight (Node _ c1 _ _ _) (Node _ c2 _ _ _) = compare c1 c2 
compareWeight t t1 = compare t t1

weight :: Tree a -> Int
weight (Node _ w _ _ _) = w
weight _ = 0

addWeight :: Tree a -> Int -> Tree a
addWeight (Node a c1 h l r) c2 = (Node a (c1 + c2) h l r) 
addWeight x _ = x

heuristic :: Tree a -> Int
heuristic (Node _ _ h _ _) = h
heuristic _ = 0

addHeuristic :: Tree a -> Int -> Tree a
addHeuristic (Node a c h1 l r) h2 = (Node a c (h1 + h2) l r) 
addHeuristic x _ = x


leftChild :: Tree a -> Tree a
leftChild Void = Void
leftChild (Node _ _ _ left _) = left

rightChild :: Tree a -> Tree a
rightChild Void = Void
rightChild (Node _ _ _ _ right) = right

generateChildren :: (Eq a) => Tree a -> [Tree a]
generateChildren t = filter (/= Void) [leftChild t, rightChild t] 

generateUniqueChildren :: (Eq a) => Tree a -> [Tree a] -> [Tree a]
generateUniqueChildren t list = filter (`notElem` list) (generateChildren t) 
