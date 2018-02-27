module Parser ( parseExpression,
                testnodes,
                Atom(..), 
                Operator(..), 
                MTree(..)) 
                where

import Control.Monad
import Control.Applicative

{-
    Author          Jan Richter
    Date            27.02.2018
    Description    'parseExpression' parses strings of propositional logical expressions.
                    The function generates an expression tree of the following structure:
                    Tree := Leaf Atom | Node Operator [Tree]. Negation does count as 
                    Operator and contains the single negated expression node.

    Input           All tokens have to be separated by whitespace, with the exception of 
    Restrictions    brackets. Variables have to start with a letter, but can be of any 
                    length and can contain numbers. Contradictions and tautologies are 
                    defined by "0" and "1". 

    Grammar         Equivalence := Implication | Implication <-> Equivalence 
                    Implication := Disjunction | Disjunction ->  Implication 
                    Disjunction := Conjunction | Conjunction |  Disjunction 
                    Conjunction := Atom        | Atom        &  Conjunction 
                    Atom        := Boolean | Variable | (Equivalence) | ! Atom 
-}

{-
    Functions Intended For External Use
-}

parseExpression :: String -> MTree 
parseExpression = fst . head . parse equivalence  

{-
    Object & Constant Definitions

    The parsing function of Parser Objects should generate empty lists if the parser 
    does not succeed. If the parsing function is non-deterministic the function yields 
    all possible results.
-}

data Parser a    = MParser (String -> [(a, String)]) 

data Atom        = Val Bool | Var String 
data Operator    = Negate | And | Or | Impl | Equiv deriving (Eq)

data MTree       = Leaf Atom | Node Operator [MTree]

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
    (treeToString n) ns

testnodes :: MTree -> String 
testnodes (Leaf a) = atomToString a 
testnodes (Node op ns) = 
    opToString op ++ " (" ++ 
    (foldl (\x y -> x ++ " ," ++ testnodes y) "" ns) 
    ++ ")"

{-
    Convenience Functions
-}

isLeaf :: MTree -> Bool 
isLeaf (Leaf _)          = True 
isLeaf _                 = False

-- splits a string after a token
token :: String -> (String,String) 
token ""           = ("","")
token (' ':xs)     = ("",xs) 
token ('!':xs)     = ("!",xs)
token ('(':xs)     = ("(",xs)
token (')':xs)     = ("",')':xs)
token (x:xs)       = (,) ([x] ++ (fst $ token xs)) (snd $ token xs)

{-
    Monadic Parser Instance Definitions
-}
 
instance Monad Parser where 
    return a = MParser $ \s -> [(a,s)] 
    p >>= f  = MParser $ \s -> [(a,r) | 
        (a',s') <- parse p s, 
        (a,r)   <- parse (f a') s']

instance Applicative Parser where 
    pure a   = MParser $ \s -> [(a,s)]
    f <*> p  = MParser $ \s -> [(g a,r) | 
        g     <- [ a' | (a',s') <- parse f s], 
        (a,r) <- parse p s]

instance Functor Parser where 
    fmap f p = MParser $ \s -> [(f a,r) | (a,r) <- parse p s] 

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

instance MonadPlus Parser where 
    mzero     = MParser $ \s -> []
    mplus p q = MParser $ \s -> (parse p s) ++ (parse q s)

{-
    General Parser Functions
-}

parse :: Parser a -> String -> [(a,String)]
parse (MParser p) = p 

peek :: Parser String
peek = MParser $ \s -> [(fst $ token s,s)]

pop :: Parser () 
pop = MParser $ \s -> [((), snd $ token s)]

{-
    Tree Generating Parser Functions
    (correspond to the grammar specified in the module description)
-}

equivalence :: Parser MTree 
equivalence = do 
    t1 <- implication
    op <- peek 
    case op of 
        "<->" -> do 
                  pop 
                  t2 <- equivalence
                  return $ Node Equiv $ [t1] ++ [t2]
        _     -> return t1 

implication :: Parser MTree 
implication = do 
    t1 <- disjunction 
    op <- peek 
    case op of 
        "->" -> do 
                 pop 
                 t2 <- implication 
                 return $ Node Impl $ [t1] ++ [t2]
        _    -> return t1

disjunction :: Parser MTree 
disjunction = do 
    t1 <- conjunction
    op <- peek 
    case op of 
        "|" -> do 
                pop 
                t2 <- disjunction
                return $ Node Or $ [t1] ++ [t2]
        _   -> return t1 

conjunction :: Parser MTree 
conjunction = do 
    t1 <- atom 
    op <- peek 
    case op of 
        "&" -> do  
                pop
                t2 <- conjunction 
                return $ Node And $ [t1] ++ [t2]
        _   -> return t1 


atom :: Parser MTree 
atom = do 
    val <- peek 
    pop
    case val of 
        " " -> do 
                atom
        "(" -> do 
                t <- equivalence
                pop 
                return t 
        "!" -> do 
                t <- atom
                return $ Node Negate [t]
        "0" -> return $ Leaf $ Val False 
        "1" -> return $ Leaf $ Val True 
        _   -> return $ Leaf $ Var val 
