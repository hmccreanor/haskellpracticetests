-- Gave up exam conditions with 45 mins to go
module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp x xs
  = head [b | (a, b) <- xs, a == x]

-- 3 marks
vars :: Formula -> [Id]
vars (Var x)
  = [x]
vars (Not f)
  = (sort . nub . vars) f
vars (And f f')
  = (sort . nub) (v ++ v')
  where
    v = vars f
    v' = vars f'
vars (Or f f')
  = (sort . nub) (v ++ v')
  where
    v = vars f
    v' = vars f'

-- 1 mark
idMap :: Formula -> IdMap
idMap f
  = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF f@(Var _)
  = f
toNNF (Not (And f f'))
  = Or (toNNF $ Not f) (toNNF $ Not f')
toNNF (Not (Or f f'))
  = And (toNNF $ Not f) (toNNF $ Not f')
toNNF (Not (Not f))
  = toNNF f
toNNF (Not f)
  = Not (toNNF f)
toNNF (And f f')
  = And (toNNF f) (toNNF f')
toNNF (Or f f')
  = Or (toNNF f) (toNNF f')

-- 3 marks
toCNF :: Formula -> CNF
toCNF 
  = toCNF' . toNNF
  where
    toCNF' :: NNF -> CNF
    toCNF' (Or f f')
      = distribute f f'
    toCNF' (And f f')
      = And (toCNF' f) (toCNF' f')
    toCNF' x = x

-- 4 marks
flatten :: CNF -> CNFRep
flatten f
  = flatten' f
  where
    m = idMap f
    flatten' :: CNF -> CNFRep
    flatten' (Var x)
      = [[lookUp x m]]
    flatten' (Not (Var x))
      = [[negate $ lookUp x m]]
    flatten' (Or f f')
      = [concat $ flatten' f ++ flatten' f']
    flatten' (And f f')
      = flatten' f ++ flatten' f'

--------------------------------------------------------------------------
-- Part III

-- 5 marks
{-}
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits xs
  = (prop units (units xs), (units xs))
  where
    units = concat . filter ((==1) . length) 
    prop :: [Int] -> CNFRep -> CNFRep
    prop [] xs
      = xs
    prop (u : us) xs
      = prop us xs'
      where
        xs' = map (filter (/= (-u))) $ filter (not . (elem u)) xs
        -}

-- 4 marks
dp :: CNFRep -> [[Int]]
dp 
  = undefined

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined


