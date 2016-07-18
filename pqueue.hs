{-# LANGUAGE ViewPatterns #-}

-- Written by Annie Cherkaev and Sean Martin

import Test.QuickCheck
import Test.QuickCheck.Function


data SkewHeap a = Empty | Node (SkewHeap a) a (SkewHeap a) deriving (Eq,Show)

singleton :: Ord a => a -> SkewHeap a
singleton x = insert x Empty

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x h = merge (Node Empty x Empty) h


merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge Empty h = h
merge h Empty = h
merge h1@(Node l1 v1 r1) h2@(Node l2 v2 r2) | v1 < v2 = Node (merge r1 h2) v1 l1
                                            | otherwise = Node (merge r2 h1) v2 l2


maxDepth :: SkewHeap a -> Int
maxDepth Empty = 0
maxDepth (Node l _ r) = 1 +  max (maxDepth l) (maxDepth r)

minDepth :: SkewHeap a -> Int
minDepth Empty = 0
minDepth (Node l _ r) = 1 +  min (minDepth l) (minDepth r)

getMin :: Bounded a => SkewHeap a -> a
getMin (Node l x r) = x
getMin Empty = maxBound

-- Typeclass instances:

instance Functor SkewHeap where
  fmap _ Empty = Empty
  fmap f (Node Empty x Empty) = Node Empty (f x) Empty
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)


instance (Ord a, Arbitrary a) => Arbitrary (SkewHeap a) where
  arbitrary = sized arbitrarySkewHeap

arbitrarySkewHeap :: (Ord a, Arbitrary a) => Int -> Gen (SkewHeap a)
arbitrarySkewHeap 0 = return Empty
arbitrarySkewHeap size = do
  t <- arbitrary -- generates an arbitrary a
  newSize <- choose (0, size - 3 )
  ts <- arbitrarySkewHeap newSize
  return $ merge (Node Empty t Empty) ts



-- Testing Functor laws:

prop_functorID :: (Eq a, Ord a, Num a) => SkewHeap a -> Bool
prop_functorID h = fmap id h == h

prop_functorComposition :: (Eq c, Num c) => SkewHeap a -> Fun b c -> Fun a b -> Bool
prop_functorComposition h (apply -> f) (apply -> g) = (fmap (f . g) h) == (fmap f (fmap g h))

-- Technically works but probably don't use:
prop_functorComposition' :: SkewHeap Char -> Fun Int Integer-> Fun Char Int -> Bool
prop_functorComposition' h (apply -> f) (apply -> g) = (fmap (f . g) h) == (fmap f (fmap g h))


-- Basic tests for the tree generating functions

prop_heap :: SkewHeap Int -> Bool
-- prop_heap [] = True
-- prop_heap [x] = True
-- prop_heap l@(x:xs) = (foldr min x xs) == (getMin $ fromList l)

prop_heap Empty = True
prop_heap (Node Empty _ Empty) = True
prop_heap (Node l x r) = (x <= getMin l) && (x <= getMin r) && (prop_heap l) && (prop_heap r)

prop_heap' :: SkewHeap Char -> Bool
prop_heap' Empty = True
prop_heap' (Node Empty _ Empty) = True
prop_heap' (Node l x r) = (x <= getMin l) && (x <= getMin r) && (prop_heap' l) && (prop_heap' r)

-- Example of failing test:
prop_depth :: Int -> Bool
prop_depth n = (maxDepth $ fromInt n) == n + 1

-- Semi-Working version (very slow. poss. constrain input sizes?):
prop_depth' :: Int -> Property
prop_depth' n = ((abs n) < 10) ==> (maxDepth $ fromInt n) == (abs n) + 1

 -- Utility functions:

fromInt :: Int -> SkewHeap Int
fromInt n = fromInt' n (abs n)
 where fromInt' c d | d <= 0 = Node Empty c Empty
                    | d > 0 = Node (fromInt' (c+1) (d-1)) c (fromInt' (c+1) (d-1))

fromList :: (Ord a, Arbitrary a) => [a] -> SkewHeap a
fromList (x:xs) = foldr insert (singleton x) xs

-- printing solution from:
-- http://stackoverflow.com/questions/12556469/nicely-printing-showing-a-binary-tree-in-haskell
prettyprint Empty = "Empty root."
prettyprint (Node left node right) = unlines (prettyprint_helper (Node left node right))

prettyprint_helper (Node left node right) = (show node) : (prettyprint_subtree left right)
        where pad first rest = zipWith (++) (first : repeat rest)
              prettyprint_subtree left right = ((pad "+- " "|  ") (prettyprint_helper right)) ++ ((pad "`- " "   ") (prettyprint_helper left))
prettyprint_helper Empty = []

printHeap :: Show a => SkewHeap a -> IO ()
printHeap = putStrLn . prettyprint
