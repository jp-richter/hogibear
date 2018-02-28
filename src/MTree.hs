module MTree (MTree(..), 
              Atom(..), 
              Operator(..),
              testnodes,
              isLeaf,
              children,
              hasOperator,
              isConjunction,
              isDisjunction,
              splitAtOp) where 

{-
    Author          Jan Richter
    Date            27.02.2018
    Description     This module provides a tree for propositional logic 
                    expressions, with simple functions to manipulate and 
                    read its nodes. testnodes is for testing purposes.
-}

{-
    Tree Definitions
-}

data Atom        = Val Bool | Var String deriving (Eq)
data Operator    = Negate | And | Or | Impl | Equiv deriving (Eq)

data MTree       = Leaf Atom | Node Operator [MTree]

{-
    Functions Intended For External Use
-}

children :: MTree -> [MTree]
children n@(Leaf _)  = []
children (Node _ ns) = ns

hasOperator :: MTree -> Operator -> Bool
hasOperator (Node op _) op' = op == op' 
hasOperator _ _             = False

isLeaf :: MTree -> Bool 
isLeaf (Leaf _) = True 
isLeaf _        = False

splitAtOp :: [MTree] -> Operator -> ([MTree],[MTree]) 
splitAtOp [] _      = ([],[]) 
splitAtOp (n:ns) op = case n of 
    _ | hasOperator n op -> ([n], ns)
      | otherwise        -> (fst $ splitAtOp ns op
                            , n:(snd $ splitAtOp ns op))

isDisjunction :: MTree -> Bool 
isDisjunction (Node Or _) = True 
isDisjunction _           = False

isConjunction :: MTree -> Bool
isConjunction (Node And _) = True 
isConjunction _            = False

{-
    Tree Visualization
-}

instance Show MTree where
    show t = show $ treeToString t

atomToString :: Atom -> String 
atomToString (Val True)  = "True"
atomToString (Val False) = "False"
atomToString (Var s)     = s

opToString :: Operator -> String 
opToString Negate = "!"
opToString And    = "&"
opToString Or     = "|"
opToString Impl   = "->"
opToString Equiv  = "<->"

treeToString :: MTree -> String 
treeToString (Leaf a)          = atomToString a 
treeToString (Node Negate [n]) = "!" ++ (treeToString n) 
treeToString (Node op (n:ns))  = foldl (\x y -> 
    if isLeaf y then x ++ " " ++ (opToString op) ++ " " ++ treeToString y
    else x ++ " " ++ (opToString op) ++ " (" ++ (treeToString y) ++ ")") 
    (f n) ns where 
        f n = if isLeaf n then treeToString n
              else "(" ++ (treeToString n) ++ ")"

testnodes :: MTree -> String 
testnodes (Leaf a) = atomToString a 
testnodes (Node op ns) = 
    opToString op ++ " (" ++ 
    (foldl (\x y -> x ++ " ," ++ testnodes y) "" ns) 
    ++ ")"