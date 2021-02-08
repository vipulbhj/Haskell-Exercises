-- Creating our own data types in Haskell

data Tree = Leaf | Node Int Tree Tree deriving Show

depthTree :: Tree -> Int
depthTree Leaf = 0
depthTree (Node _ leftSubTree rightSubTree) = 
  1 + max (depthTree leftSubTree) (depthTree rightSubTree)


treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node val leftSubTree rightSubTree) = 
 val + treeSum leftSubTree + treeSum rightSubTree


treeToList :: Tree -> [Int]
treeToList Leaf = []
treeToList (Node val leftSubtree rightSubtree) = 
  treeToList leftSubtree ++ [val] ++ treeToList rightSubtree
 