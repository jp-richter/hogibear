module Satisfiability where 

import Normalforms
import MTree

hornSat :: MTree -> [String]
hornSat = (markAlg []) . toImplicationForm

renamableHornSat :: MTree -> [(String, String)]

markAlg :: [String] -> MTree -> [String] 
markAlg vs (Node And ns)  = 
markAlg vs (Node Impl ns) = 

nextNode :: MTree -> 
