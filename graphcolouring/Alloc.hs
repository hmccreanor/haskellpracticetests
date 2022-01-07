module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x 
  = length . filter (==x)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (ns, es)
  = [(n, (count n e) + (count n e')) | n <- ns]
  where
    (e, e') = unzip es


neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (_, es)
  = [if e' == n then e else e' | (e, e') <- es, n == e || n == e']

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n (ns, es)
  = (filter (/=n) ns, [x | x@(e, e') <- es, e /= n && e' /= n])

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([], _)
  = []
colourGraph k g
  | null cs   = (n, 0) : cMap
  | otherwise = (n, head cs) : cMap
  where
    ((n, _) : _) = map flipTuple . sort . map flipTuple $ degrees g
    g'      = removeNode n g
    cMap    = colourGraph k g'
    ncs     = [lookUp x cMap | x <- neighbours n g]
    cs      = [1..k] \\ ncs

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap 
  = undefined

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments 
  = undefined

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp 
  = undefined

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock 
  = undefined

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG 
  = undefined

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined
