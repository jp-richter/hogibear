module Normalforms (toNNF, toDNF, toCNF, isHornForm, toImplicationForm) where

import Prelude hiding (negate) 
import MTree

{-
    Author          Jan Richter
    Date            27.02.2018
    Description     This module provides functions to transform MTrees into 
                    normal forms. 

    TODO aussagenlogik abstrahieren
-}

{-
    Functions Intended For External Use
-}

toNNF :: MTree -> MTree 
toNNF = dissolveBrackets .
        dissolveNegation . 
        dissolveImplication . 
        dissolveEquivalence

toDNF :: MTree -> MTree 
toDNF = toDNF' . dissolveBrackets where 
    toDNF' n 
        | isDNF n   = n 
        | isNNF n   = toDNF $ pullOutOr n 
        | otherwise = toDNF' $ toNNF n

toCNF :: MTree -> MTree 
toCNF = toCNF' . dissolveBrackets where 
    toCNF' n 
        | isCNF n   = n 
        | isNNF n   = toCNF $ pullOutAnd n
        | otherwise = toCNF' $ toNNF n 

isHornForm :: MTree -> Bool
isHornForm (Node And ns) = maximum (map f ns) <= 1 where 
    f (Leaf (Val False)) = 0
    f (Node Negate n)    = 0
    f (Leaf (Val True))  = 1
    f (Leaf (Var _))     = 1
    f (Node Or (n:ns))   = (f n) + (f (Node Or ns))
    f _                  = 2
isHornForm _             = False

toImplicationForm :: MTree -> MTree
toImplicationForm (Node And ns) = Node And $ map ressolveImplication ns
--toImplicationForm n             = toImplicationForm $ toCNF $ toNNF n

{-
    Convencience Functions
-}

isNNF :: MTree -> Bool 
isNNF (Leaf l)          = True
isNNF (Node Negate [n]) = isLeaf n 
isNNF (Node _ ns)       = and $ map isNNF ns 

isDNF :: MTree -> Bool 
isDNF (Leaf l)         = True 
isDNF (Node And ns)    = and $ map hasOnlyLeaf ns
isDNF (Node Or ns)     = and $ map isDNF ns 
isDNF (Node Negate ns) = and $ map isDNF ns 
isDNF (Node _ _)       = False

isCNF :: MTree -> Bool 
isCNF (Leaf l)         = True 
isCNF (Node Or ns)     = and $ map hasOnlyLeaf ns  
isCNF (Node And ns)    = and $ map isCNF ns 
isCNF (Node Negate ns) = and $ map isCNF ns 
isCNF (Node _ _)       = False

allNegatives :: [MTree] -> [MTree]
allNegatives = filter (not . isPositive)

allPositives :: [MTree] -> [MTree]
allPositives = filter isPositive

-- assumes argument does not have child operators
isPositive :: MTree -> Bool 
isPositive (Leaf (Val True))  = True 
isPositive (Leaf (Val False)) = False 
isPositive (Leaf _ )          = True
isPositive (Node Negate [n])  = not $ isPositive n 

{-
    Tree Transforming Functions
-}

negate :: MTree -> MTree 
negate n@(Leaf _)                   = Node Negate [n]
negate (Node Negate [n@(Leaf _)])   = n 
negate (Node Negate [n@(Node _ _)]) = n
negate n@(Node _ _)                 = Node Negate [n] 
 
dissolveEquivalence :: MTree -> MTree 
dissolveEquivalence n@(Leaf _)            = n 
dissolveEquivalence (Node Equiv (n:m:[])) = 
    let left = Node Impl [n,m]
        right = Node Impl [m,n]
    in  Node And [left, right]
dissolveEquivalence (Node Equiv ns)       = 
    let n = head ns 
        m = Node Equiv $ tail ns
        left = Node Impl [n,m]
        right = Node Impl [m,n]
        left' = dissolveEquivalence left 
        right' = dissolveEquivalence right
    in  Node And [left', right'] 
dissolveEquivalence (Node op ns)          = 
    Node op $ map dissolveEquivalence ns

dissolveImplication :: MTree -> MTree 
dissolveImplication n@(Leaf _)     = n 
dissolveImplication (Node Impl ns) = Node Or $ f [] ns where 
    f xs [y]     = xs ++ [y]
    f xs (y:ys)  = f (map negate $ xs ++ [y]) ys 
dissolveImplication (Node op ns)   = Node op $ map dissolveImplication ns 

-- assumes argument does not contain equivalences & implications
dissolveNegation :: MTree -> MTree 
dissolveNegation (Node Negate [(Node And ns)])     = 
    Node Or $ map (dissolveNegation . negate) ns
dissolveNegation (Node Negate [(Node Or ns)])      = 
    Node And $ map (dissolveNegation . negate) ns 
dissolveNegation (Node Negate [(Node Negate [n])]) = n 
dissolveNegation n                                 = n 

ressolveImplication :: MTree -> MTree 
ressolveImplication (Node Or ns) = 
    let premise   = if (length $ allNegatives ns) == 0 
                        then Leaf (Val True); 
                    else Node And $ map negate $ allNegatives ns
        conclusio = if (length $ allPositives ns) == 0 
                        then Leaf (Val False); 
                    else Node And $ allPositives ns 
    in Node Impl [premise, conclusio]
   
-- assumes argument is in negation normal form / -> dnf
pullOutOr :: MTree -> MTree
pullOutOr n@(Leaf _)      = n
pullOutOr n@(Node And ns) = 
    if not $ and $ map (flip hasOp And) ns 
        then let (Node And ns) = dissolveBrackets n
                 disjunction   = head $ fst $ splitAtOp ns Or 
                 rest          = snd $ splitAtOp ns Or
                 toConjunct1   = head $ children disjunction
                 toConjunct2   = tail $ children disjunction
                 conjunction1  = Node And $ rest ++ [toConjunct1]
                 conjunction2  = Node And $ rest ++ toConjunct2
             in Node Or [conjunction1, conjunction2]
    else Node And $ map pullOutOr ns
pullOutOr (Node op ns)     = Node op $ map pullOutOr ns

-- assumes argument is in negation normal form / -> cnf
pullOutAnd :: MTree -> MTree 
pullOutAnd n@(Leaf _)     = n
pullOutAnd n@(Node Or ns) = 
    if not $ or $ map (flip hasOp Or) ns 
        then let (Node Or ns)  = dissolveBrackets n
                 conjunction   = head $ fst $ splitAtOp ns And 
                 rest          = snd $ splitAtOp ns And
                 toDisjunct1   = head $ children conjunction
                 toDisjunct2   = tail $ children conjunction
                 disjunction1  = Node Or $ rest ++ [toDisjunct1]
                 disjunction2  = Node Or $ rest ++ toDisjunct2
             in Node And [disjunction1, disjunction2]
    else Node Or $ map pullOutAnd ns
pullOutAnd (Node op ns)   = Node op $ map pullOutAnd ns

dissolveBrackets :: MTree -> MTree 
dissolveBrackets n@(Leaf _)    = n 
dissolveBrackets (Node op [n])  
    | hasOp n op = dissolveBrackets $ Node op $ children n 
    | otherwise        = Node op $ [dissolveBrackets n]
dissolveBrackets (Node op (n:ns)) 
    | hasOp n op = dissolveBrackets $ Node op $ ns ++ (children n)
    | otherwise        = Node op $ n:(map dissolveBrackets ns)
