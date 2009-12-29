{---------------------------------------------
 - Ramin Rakhamimov
 - CIS 716
 - Search implementations in Haskell.
 -
 - Execution:
 -   Dfs: [Goal,'E','J','I','D','B','A']
 -   Bfs: [Goal,'D','E','F','H','B','C','A']
 -   HillClimb: [Goal,'H','F','C','A']
 -   BranchAndBound: [Goal,'H','C','B','A']
 -   A*: [Goal,'H','F','C','A']
 --------------------------------------------}
 
import Tree
import Dfs
import Bfs
import HillClimb
import BranchAndBound
import AStar

input = Node 'A' 0 18 (Node 'B' 4 14 (Node 'D' 15 9 (Node 'I' 12 21 (Void) (Void)) 
                                                    (Node 'J' 10 31 (Void) (Void))
                                     ) 
                                     (Node 'E' 13 12 (Goal)
                                                     (Node 'K' 16 34 (Void) (Void))
                                     )
                      ) 
                      (Node 'C' 11 4 (Node 'F' 4 1 (Node 'L' 6 21 (Void) (Void)) 
                                                    (Node 'M' 3 18 (Void) (Void))
                                     ) 
                                     (Node 'H' 3 3 (Node 'N' 1 8 (Void) (Void))
                                                   (Goal)
                                     )
                      ) 

main = do
    putStr "Dfs: "
    print $ dfs input
    putStr "Bfs: "
    print $ bfs input
    putStr "HillClimb: "
    print $ hillClimb input
    putStr "BranchAndBound: "
    print $ branchAndBound input
    putStr "A*: "
    print $ aStar input
