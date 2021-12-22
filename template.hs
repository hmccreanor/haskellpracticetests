data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix "" _
  = True
isPrefix _ ""
  = False
-- I feel like this next bit can be improved
isPrefix (a : as) (b : bs)
  | a == b = isPrefix as bs
  | otherwise = False

removePrefix :: String -> String -> String
removePrefix s
--Pre: s is a prefix of s'
  = drop (length s) 

suffixes :: [a] -> [[a]]
suffixes xs
  = take n $ iterate tail xs 
  where
    n = length xs

isSubstring :: String -> String -> Bool
isSubstring s s'
  = any (isPrefix s) (suffixes s')

findSubstrings :: String -> String -> [Int]
findSubstrings s s'
  = (map snd) . (filter ((isPrefix s) . fst)) $ zip (suffixes s') [0..] 

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf x)
  = [x]
getIndices (Node xs)
  = concatMap (getIndices . snd) xs

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition a b
  = partition' ([], a, b)
  where 
    partition' :: Eq a => ([a], [a], [a]) -> ([a], [a], [a])
    partition' t@(p, (a : as), (b : bs))
      | a == b = partition' (p ++ [a], as, bs)
      | otherwise = t
    partition' t@(p, _, _)
      = t

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf x)
  = [x]
findSubstrings' s (Leaf _)
  = []
findSubstrings' s (Node xs)
  = concatMap (findSubstrings'' s) xs
  where
    findSubstrings'' :: String -> (String, SuffixTree) -> [Int]
    findSubstrings'' s (a, sfs)
      | isPrefix s a = getIndices sfs
      | isPrefix a s = findSubstrings' (removePrefix a s) sfs 
      | otherwise = []

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
--Pre: we only try to insert into a Node
insert (s, n) (Node xs)
  = (Node . insert') xs
  where
    insert' :: [(String, SuffixTree)] -> [(String, SuffixTree)]
    insert' []
      = [(s, Leaf n)]
    insert' (x@(a, t) : xs)
      = case partition a s of
          ("", _, _) -> x : insert' xs
          (p, "", s') -> (s', insert (s', n) t) : xs
          (p, a', s') -> (p, Node [(s', Leaf n), (a', t)]) : xs
          

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]


