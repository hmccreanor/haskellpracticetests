module Alloc where

import Data.Maybe
import Data.List
import Data.Tuple

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
    ((n, _) : _) = map swap . sort . map swap $ degrees g
    cMap    = colourGraph k $ removeNode n g
    ncs     = [lookUp x cMap | x <- neighbours n g]
    cs      = [1..k] \\ ncs

------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap m
  = ("return", "return") : map buildId m 
  where
    buildId :: (Id, Int) -> (String, String)
    buildId (v, 0) 
      = (v, v)
    buildId (v, n)
      = (v, "R" ++ show n)

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments xs m
  = map (\x -> Assign (lookUp x m) (Var x)) xs

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Var x) m
  = Var $ lookUp x m
renameExp (Apply o e e') m
  = Apply o (renameExp e m) (renameExp e' m)
renameExp x _
  = x

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock [] _
  = []
renameBlock ((Assign i (Var i')) : xs) m
  | ri == ri'   = xs'
  | otherwise = (Assign ri (Var ri')) : xs'
  where
    ri = lookUp i m
    ri' = lookUp i' m
    xs' = renameBlock xs m
renameBlock ((Assign i e) : xs) m
  = (Assign i (renameExp e m)) : (renameBlock xs m)
renameBlock ((If e b b') : xs) m
  = (If (renameExp e m) (renameBlock b m) (renameBlock b' m)) : (renameBlock xs m) 
renameBlock ((While e b) : xs) m
  = (While (renameExp e m) (renameBlock b m)) : (renameBlock xs m)

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG vss
  = (nub ns, nub es)
  where
    (ns, es) = foldl buildIG' ([], []) vss
    buildIG' :: ([Id], [Edge Id]) -> [Id] -> ([Id], [Edge Id])
    buildIG' (ins, ies) vs
      = (vs ++ ins, [(min x y, max x y) | x <- vs, y <- vs, x /= y] ++ ies)
    

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
