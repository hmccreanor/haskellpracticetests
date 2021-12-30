-- Finished trying to make improvements with 27 mins to go

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

key :: BinTree a -> a
key (Node k _ _)
  = k

rank :: BinTree a -> Int
rank (Node _ r _)
  = r

children :: BinTree a -> [BinTree a]
children (Node _ _ c)
  = c

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
--Pre: both trees have the same rank
combineTrees t@(Node k r c) t'@(Node k' r' c')
  | k < k'    = Node k (r + 1) (t' : c)
  | otherwise = Node k' (r + 1) (t : c')

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
--Pre: BinHeap contains at least one element
extractMin 
  = minimum . map key

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] h
  = h
mergeHeaps h []
  = h
mergeHeaps h@(t : ts) h'@(t' : ts')
  | r < r'    = t  : mergeHeaps ts h'
  | r' < r    = t' : mergeHeaps h ts'
  | otherwise = mergeHeaps [combineTrees t t'] $ mergeHeaps ts ts'
  where
    r  = rank t
    r' = rank t'

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert x h
  = mergeHeaps h [Node x 0 []]

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h
  = mergeHeaps h' $ reverse c
  where
    ((Node _ _ c), h') = removeMin h

-- Not sure if you can do this with HoF as you can only remove one BinTree
remove :: Eq a => a -> BinHeap a -> BinHeap a
remove _ []
  = []
remove x (t@(Node k _ _) : ts)
  | k == x = ts
  | otherwise = t : remove x ts

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin h
  = (head $ filter ((== m) . key) h, remove m h)
  where
    m = extractMin h

binSort :: Ord a => [a] -> [a]
binSort xs
  = unfoldr $ foldl (flip insert) [] xs
{-}
binSort xs
  = clearHeap $ foldl (flip insert) [] xs
  where 
    clearHeap :: Ord a => BinHeap a -> [a]
    clearHeap [] 
      = []
    clearHeap ys
      = extractMin ys : (clearHeap $ deleteMin ys)
-}

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary []
  = [0]
toBinary h
  = [if x `elem` ranks then 1 else 0 | x <- [m, m-1 .. 0]]
  where
    ranks = map rank h
    m = maximum ranks

paddedZip :: [a] -> [b] -> a -> b -> [(a, b)]
paddedZip xs ys xp yp
  | xl < yl   = zip (take d $ repeat xp) (take d ys) ++ zip xs (drop d ys)
  | yl < xl   = zip (take d xs) (take d $ repeat yp) ++ zip (drop d xs) ys
  | otherwise = zip xs ys
  where
    xl = length xs
    yl = length ys
    d = abs $ xl - yl

binarySum :: [Int] -> [Int] -> [Int]
binarySum b b'
  | c == 1 = 1 : added
  | otherwise = added
  where
    rippleAdd :: (Int, Int) -> ([Int], Int) -> ([Int], Int)
    rippleAdd (x, y) (xs, c)
      = ((x + y + c) `mod` 2 : xs, (x + y + c) `div` 2)
    (added, c) = foldr rippleAdd ([], 0) $ paddedZip b b' 0 0

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]



