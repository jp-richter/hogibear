import Prelude hiding (negate) 
import Parser

-- TODO Fehlerbehandlung bei so ziemlich allem, oder zumindest für alles Voraussetzungen benennen

-- Functions intended for external use

toNNF :: MTree -> MTree 
toNNF = negations . implications . equivalences

toCNF :: MTree -> MTree -- assumes nnf
toCNF n = if isCNF n then n; else toCNF $ distributiveOr n  

-- Helper Functions

negate :: MTree -> MTree 
negate n@(Leaf _)                   = Node Negate [n]
negate (Node Negate [n@(Leaf _)])   = n 
negate (Node Negate [n@(Node _ _)]) = n
negate n@(Node _ _)                 = Node Negate [n] 

-- umschreiben mit einer regel und eine funktion die rekursiv anwendet bis keine mehr da sind
equivalences :: MTree -> MTree 
equivalences n@(Leaf _)             = n 
equivalences (Node Equiv (n:m:[]))  = Node And [Node Impl [n,m], Node Impl [m,n]]
equivalences (Node Equiv (n:ns))    = Node And [Node Impl [n, equivalences $ Node Equiv ns]
                                              , Node Impl [equivalences $ Node Equiv ns, n]]
equivalences (Node op ns)           = Node op $ map equivalences ns 

implications :: MTree -> MTree 
implications n@(Leaf _)     = n 
implications (Node Impl ns) = Node Or $ f [] ns where 
                                  f xs []     = xs
                                  f xs (y:ys) = f (map negate $ y:xs) ys 
implications (Node op ns)   = Node op $ map implications ns 

negations :: MTree -> MTree -- assumes Equivalence and Implication have been resolved
negations (Node Negate [(Node And ns)])     = Node Or $ map (negations . negate) ns
negations (Node Negate [(Node Or ns)])      = Node And $ map (negations . negate) ns 
negations (Node Negate [(Node Negate [n])]) = n 
negations n                                 = n 

isCNF :: MTree -> Bool -- assumes is CNF when And n does not have Or childs
isCNF (Leaf l)      = True 
isCNF (Node And ns) = foldl (\x y -> x && (not $ isDisjunction y) && isCNF y) True ns 
isCNF (Node _ ns)   = foldl (\x y -> x && isCNF y) True ns

isDisjunction :: MTree -> Bool 
isDisjunction (Node Or _) = True 
isDisjunction _           = False

isConjunction :: MTree -> Bool
isConjunction (Node And _) = True 
isConjunction _            = False
   
distributiveOr :: MTree -> MTree -- assumes form is NNF + ?
distributiveOr n@(Leaf _)      = n
distributiveOr n@(Node And ns) = 
    if not $ and $ map (flip hasOperator And) ns 
        then let (Node And ns) = associativity n
                 disjunction   = head $ fst $ extractOperator ns Or 
                 rest          = snd $ extractOperator ns Or
                 toConjunct1   = head $ children disjunction
                 toConjunct2   = tail $ children disjunction
                 conjunction1  = Node And $ rest ++ [toConjunct1]
                 conjunction2  = Node And $ rest ++ toConjunct2
             in Node Or [conjunction1, conjunction2]
    else Node And $ map distributiveOr ns
distributiveOr (Node op ns)     = Node op $ map distributiveOr ns

distributiveAnd :: MTree -> MTree -- assumes form is NNF + ?
distributiveAnd n@(Leaf _)     = n
distributiveAnd n@(Node Or ns) = 
    if not $ or $ map (flip hasOperator Or) ns 
        then let (Node Or ns)  = associativity n
                 conjunction   = head $ fst $ extractOperator ns And 
                 rest          = snd $ extractOperator ns And
                 toDisjunct1   = head $ children conjunction
                 toDisjunct2   = tail $ children conjunction
                 disjunction1  = Node Or $ rest ++ [toDisjunct1]
                 disjunction2  = Node Or $ rest ++ toDisjunct2
             in Node Or [disjunction1, disjunction2]
    else Node Or $ map distributiveAnd ns
distributiveAnd (Node op ns)   = Node op $ map distributiveAnd ns

associativity :: MTree -> MTree -- assumes form is NNF 
associativity n@(Leaf _)       = n 
associativity (Node op (n:ns)) = 
    if hasOperator n op 
        then associativity (Node op $ ns ++ (children n))
    else Node op $ n:(map associativity ns)

children :: MTree -> [MTree]
children n@(Leaf _)  = []
children (Node _ ns) = ns

hasOperator :: MTree -> Operator -> Bool
hasOperator (Node op _) op' = op == op' 
hasOperator _ _             = False

isLeaf :: MTree -> Bool 
isLeaf (Leaf _) = True 
isLeaf _        = False

extractOperator :: [MTree] -> Operator -> ([MTree],[MTree]) -- assumes operator exists
extractOperator [] _      = ([],[])
extractOperator (n:ns) op = 
    if hasOperator n op 
        then ([n], ns)
    else (fst $ extractOperator ns op, n:(snd $ extractOperator ns op))
