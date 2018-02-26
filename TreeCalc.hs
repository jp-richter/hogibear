import Parser

toNNF :: MTree -> MTree 
toNNF 

negate :: MTree -> MTree 
negate @n(Leaf _)                 = Node Negate n 
negate (Node Negate [@n(Leaf _)]) = n -- TODO assert that xs has only one element
negate (Node Negate [@n(Node _)]) = n
negate @n(Node _ _)               = Node Negate n 

leafsOnly :: MTree -> Bool 
leafsOnly (Node _ nodes) = foldl (\x y -> x && (isLeaf y)) True nodes 

isLeaf :: MTree -> Bool 
isLeaf (Leaf _) = True 
isLeaf _        = False