import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (SkewHeap a) where
  arbitrary = sized arbitrarySkewHeap

arbitrarySkewHeap :: (Ord a, Arbitrary a) => Int -> Gen (SkewHeap a)
arbitrarySkewHeap 0 = return Empty
arbitrarySkewHeap size = do
  t <- arbitrary -- generates an arbitrary a
  newSize <- choose (0, size - 3 )
  ts <- arbitrarySkewHeap newSize
  return $ merge (Node Empty t Empty) ts

depth :: SkewHeap a -> Int
depth Empty = 0
depth (Node l _ r) = 1 + (depth l)

-- printHeap :: Show a => SkewHeap a -> String
-- printHeap Empty = ""
-- printHeap (Node Empty x Empty) = show x
-- printHeap h = print' h []
--   where print' (Node l v r) acc=

-- print' :: Show a => SkewHeap a -> [[a]] -> [[a]]
-- print' h@(Node l v r) acc =  (v : (head acc)) : (print' l) ++ (print' r)  : acc
-- print' Empty acc = acc

-- printing solution from:
-- http://stackoverflow.com/questions/12556469/nicely-printing-showing-a-binary-tree-in-haskell
prettyprint Empty = "Empty root."
prettyprint (Node left node right) = unlines (prettyprint_helper (Node left node right))

prettyprint_helper (Node left node right) = (show node) : (prettyprint_subtree left right)
        where pad first rest = zipWith (++) (first : repeat rest)
              prettyprint_subtree left right = ((pad "+- " "|  ") (prettyprint_helper right)) ++ ((pad "`- " "   ") (prettyprint_helper left))
prettyprint_helper Empty = []



-- fromInt :: (Ord a, Arbitrary a) => Int -> SkewHeap a

-- fromList :: (Ord a, Arbitrary a) => [a] -> SkewHeap a

data SkewHeap a = Empty | Node (SkewHeap a) a (SkewHeap a) deriving (Eq,Show)

singleton :: Ord a => a -> SkewHeap a
singleton x = insert x Empty

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
-- insert x Empty = Node Empty x Empty
insert x h = merge (Node Empty x Empty) h


merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge Empty h = h
merge h Empty = h
merge h1@(Node l1 v1 r1) h2@(Node l2 v2 r2) | v1 < v2 = Node (merge r1 h2) v1 l1
                                            | otherwise = Node (merge r2 h1) v2 l2


-- test = Node Empty 3 (Node Empty 5 Empty)
-- test' = Node (Node Empty 20 Empty) 1 Empty
