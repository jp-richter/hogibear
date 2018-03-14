module MTree (Atom(..), Operator(..), MTree(..), children, hasOp, isLeaf, 
    hasOnlyLeaf, isOr, isAnd, splitAtOp, depth) where 

{-
    Author          Jan Richter
    Date            27.02.2018
    Description     This module provides a tree for propositional logic 
                    expressions, with simple functions to manipulate and 
                    read its nodes. 

                    Negation does count as Operator and generates its own 
                    node. Unlike 'isLeaf' 'hasOnlyLeafs' allows negated leafs.
-}

{-
    Tree Definitions
-}

data Atom        = Val Bool | Var String
data Operator    = Negate | And | Or | Impl | Equiv deriving (Eq)

data MTree       = Leaf Atom | Node Operator [MTree]

{-
    Functions Intended For External Use
-}

children :: MTree -> [MTree]
children n@(Leaf _)  = []
children (Node _ ns) = ns

hasOp :: MTree -> Operator -> Bool
hasOp (Node op _) op' = op == op' 
hasOp _ _             = False

isLeaf :: MTree -> Bool 
isLeaf (Leaf _)          = True 
isLeaf _                 = False

hasOnlyLeaf :: MTree -> Bool 
hasOnlyLeaf (Leaf _ )         = True 
hasOnlyLeaf (Node Negate [n]) = hasOnlyLeaf n 
hasOnlyLeaf _                 = False

splitAtOp :: [MTree] -> Operator -> ([MTree],[MTree]) 
splitAtOp [] _   = ([],[]) 
splitAtOp (n:ns) op  
    | hasOp n op = ([n], ns)
    | otherwise  = (fst $ splitAtOp ns op, n:(snd $ splitAtOp ns op))

isOr :: MTree -> Bool 
isOr (Node Or _) = True 
isOr _           = False

isAnd :: MTree -> Bool
isAnd (Node And _) = True 
isAnd _            = False

depth :: MTree -> Int 
depth (Leaf _) = 1 
depth n@(Node _ ns) 
    | hasOnlyLeaf n = 1 
    | otherwise = (+) 1 $ maximum $ map depth ns

{-
    Tree Visualization
-}

instance Show MTree where
    show t = show $ tree t

atom :: Atom -> String 
atom (Val True)  = "True"
atom (Val False) = "False"
atom (Var s)     = s

operator :: Operator -> String 
operator Negate = "!"
operator And    = "&"
operator Or     = "|"
operator Impl   = "->"
operator Equiv  = "<->"

tree :: MTree -> String 
tree (Leaf a)          = atom a 
tree (Node Negate [n]) = "!" ++ (tree n) 
tree (Node op (n:ns))  = (first n) ++ (concatMap (rest "" op) ns)

first :: MTree -> String 
first n 
    | hasOnlyLeaf n = tree n 
    | otherwise     = "(" ++ (tree n) ++ ")"

rest :: String -> Operator -> MTree -> String 
rest s op n 
    | hasOnlyLeaf n = s ++ " " ++ (operator op) ++ " " ++ (tree n)
    | otherwise     = s ++ " " ++ (operator op) ++ " (" ++ (tree n) ++ ")"
