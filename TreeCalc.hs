import Parser

toNNF :: MTree -> MTree 
toNNF = resolveNeg . resolveImpl . resolveEqu

negate' :: MTree -> MTree 
negate' n@(Leaf _)                   = Node Negate [n]
negate' (Node Negate [n@(Leaf _)])   = n 
negate' (Node Negate [n@(Node _ _)]) = n
negate' n@(Node _ _)                 = Node Negate [n] 

-- TODO refactor
resolveEqu :: MTree -> MTree 
resolveEqu n@(Leaf _)             = n 
resolveEqu (Node Equiv (n:m:[]))  = Node And [Node Impl [n,m]
                                             , Node Impl [m,n]]
resolveEqu (Node Equiv (n:nodes)) = Node And [Node Impl [n, resolveEqu $ Node Equiv nodes]
                                             , Node Impl [resolveEqu $ Node Equiv nodes, n]]
resolveEqu (Node op nodes)        = Node op $ map resolveEqu nodes 

-- TODO refactor
resolveImpl :: MTree -> MTree 
resolveImpl n@(Leaf _)        = n 
resolveImpl (Node Impl nodes) = Node Or $ f [] nodes where 
    f xs []     = xs
    f xs (y:ys) = f (map negate' $ y:xs) ys 
resolveImpl (Node op nodes)   = Node op $ map resolveImpl nodes 

-- TODO refactor
resolveNeg :: MTree -> MTree -- assumes Equivalence and Implication have been resolved
resolveNeg (Node Negate [(Node And nodes)])     = Node Or $ map (resolveNeg . negate') nodes
resolveNeg (Node Negate [(Node Or nodes)])      = Node And $ map (resolveNeg . negate') nodes 
resolveNeg (Node Negate [(Node Negate [node])]) = node 
resolveNeg node                                 = node -- ausschreiben ?
