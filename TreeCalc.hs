import Prelude hiding (negate) 
import Parser

-- Functions intended for external use

toNNF :: MTree -> MTree 
toNNF = negations . implications . equivalences

toCNF :: MTree -> MTree 
toCNF t = let nnf = toNNF t 
          in if isCNF nnf then nnf; else toCNF . cnfStep 

-- Helper Functions

negate :: MTree -> MTree 
negate n@(Leaf _)                   = Node Negate [n]
negate (Node Negate [n@(Leaf _)])   = n 
negate (Node Negate [n@(Node _ _)]) = n
negate n@(Node _ _)                 = Node Negate [n] 

equivalences :: MTree -> MTree 
equivalences n@(Leaf _)             = n 
equivalences (Node Equiv (n:m:[]))  = Node And [Node Impl [n,m], Node Impl [m,n]]
equivalences (Node Equiv (n:nodes)) = Node And [Node Impl [n, equivalences $ Node Equiv nodes]
                                              , Node Impl [equivalences $ Node Equiv nodes, n]]
equivalences (Node op nodes)        = Node op $ map equivalences nodes 

implications :: MTree -> MTree 
implications n@(Leaf _)        = n 
implications (Node Impl nodes) = Node Or $ f [] nodes where 
                                  f xs []     = xs
                                  f xs (y:ys) = f (map negate $ y:xs) ys 
implications (Node op nodes)   = Node op $ map implications nodes 

negations :: MTree -> MTree -- assumes Equivalence and Implication have been resolved
negations (Node Negate [(Node And nodes)])     = Node Or $ map (negations . negate) nodes
negations (Node Negate [(Node Or nodes)])      = Node And $ map (negations . negate) nodes 
negations (Node Negate [(Node Negate [node])]) = node 
negations n                                    = n 

isCNF :: MTree -> Bool 
isCNF (Leaf l) = True 
isCNF (Node And nodes) = if 

cnfStep :: MTree -> MTree
