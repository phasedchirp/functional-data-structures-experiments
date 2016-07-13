data Queue a = Queue {front :: [a], back :: [a]} deriving (Show,Eq)

head' :: Queue a -> Maybe a
head' q@(Queue [] []) = Nothing
head' (Queue [] b) = Just $ head nf
  where nf = reverse b
head' (Queue f b) = Just $ head f

tail' :: Queue a -> Queue a
tail' q@(Queue [] []) = q
tail' (Queue [] b) = Queue nf []
  where nf = tail $ reverse b
tail' (Queue (f:fs) b) = Queue fs b

snoc :: Queue a -> a -> Queue a
snoc (Queue f b) x = Queue f (x:b)


isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

-- Going to be a priority queuing mechanism at some point
data Btree a b = Empty | Leaf a b | Tree (Btree a b) a b (Btree a b) deriving (Show)

insert :: (Eq b, Ord a) => a -> b -> Btree a b -> Btree a b
insert value contents Empty = Leaf value contents
insert value contents l@(Leaf a b) | value > a = Tree l value contents Empty
                                   | value < a = Tree Empty value contents l
                                   | otherwise = if contents == b then l else Tree l value contents Empty
insert value contents t@(Tree l v c r) = if value > v
                                          then Tree (insert value contents l) v c r
                                          else Tree l v c (insert value contents r)

-- treeMap' :: (a -> c) -> (b -> d) -> Btree a b -> Btree c d
-- treeMap' _ Empty = Empty
-- treeMap' f (Leaf v c) = Leaf (f v c)
-- treeMap' f (Tree l v c r) = Tree (treeMap f l) (f v c) (treeMap f r)
--
-- treeMap = treeMap' id



join :: (Eq b, Ord a) => Btree a b -> Btree a b -> Btree a b
join t Empty = t
join t1 t2 = joinHelper [t1] t2



joinHelper:: (Eq b, Ord a) => [Btree a b] -> Btree a b -> Btree a b
joinHelper [] doneTree = doneTree
joinHelper (Empty:toDo) doneTree = joinHelper toDo doneTree
joinHelper ((Leaf v c):toDo) doneTree = joinHelper toDo (insert v c doneTree)
joinHelper ((Tree l v c r):toDo) doneTree = joinHelper (l:r:toDo) (insert v c doneTree)


--                                                        | otherwise


-- Don't use probably
-- contains :: Eq b => b -> Btree a b -> Bool
-- contains x Empty = False
-- contains x (Leaf v c) = x == c
-- contains x (Tree l v c r) = if x == c then True else contains x l || contains x r

-- delete ::  b -> Btree a b -> Btree a b
-- delete x Empty = Empty
-- delete x l@(Leaf v c) = if c == x then Empty else l
-- delete x (Tree l v c r) | c == x =


-- isEmpty :: Btree a -> Bool

-- getHighest :: Btree a -> a

-- getLowest :: Btree a -> a
