{-# LANGUAGE OverloadedLists, TypeFamilies #-}
import GHC.Exts (IsList(..))
import Data.Monoid

-- data Tree a = Leaf a | Succ (Tree (Node a)) deriving (Show)


-- x = Node3 'a' 'b' 'c'
-- y = Succ $ Leaf (Node2 (Succ (Leaf x)) (Succ (Leaf x)))


data FingerTree v a = Empty
                    | Single v a
                    | Deep { annotation :: v,
                             prefix :: Digit a,
                             deeper :: FingerTree v (Node v a),
                             suffix :: Digit a
                    } deriving (Show)

data Node v a = Node2 v a a | Node3 v a a a deriving (Show)
type Digit a = [a]

infixr 5 <|
(<|) :: (Monoid a) => a -> FingerTree a a -> FingerTree a a
(<|) x Empty = Single x x
(<|) x (Single v y) = Deep (x <> v) [x] Empty [y]
(<|) x (Deep v [a,b,c,d] m rf) = Deep (v <> x) [x, a] ((Node3 (b <> c <> d) b c d) <| m) rf
(<|) x (Deep v lf m rf) = Deep (x <> v) (x:lf) m rf
--
-- infixl 6 |>
-- (|>) :: FingerTree a -> a -> FingerTree a
-- (|>) Empty x = Single x
-- (|>) (Single y) x =  Deep [y] Empty [x]
-- (|>) (Deep lf m [a,b,c,d]) x = Deep lf (m |> (Node3 a b c) ) [d,x]
-- (|>) (Deep lf m rf) x = Deep lf m (rf ++ [x])
--
-- viewl :: FingerTree a -> Maybe (a, FingerTree a)
-- viewl Empty = Nothing
-- viewl (Single x) = Just (x, Empty)
-- viewl (Deep [x] m rf) = case viewl m of Just ((Node2 a b) , deeper) -> Just (x, Deep [a,b] deeper rf)
--                                         Just ((Node3 a b c), deeper) -> Just (x, Deep [a,b,c] deeper rf)
--                                         Nothing -> case rf of [a] -> Just (x, Single a)
--                                                               [a,b] -> Just (x, Deep [a] Empty [b])
--                                                               [a,b,c] -> Just (x, Deep [a,b] Empty [c])
--                                                               [a,b,c,d] -> Just (x, Deep [a,b] Empty [c,d])
-- viewl (Deep lf m rf) = Just (head lf, Deep (tail lf) m rf)
--
-- head' :: FingerTree a -> Maybe a
-- head' t = case viewl t of (Just (x,_)) -> Just x
--                           otherwise -> Nothing
--
-- tail' :: FingerTree a -> Maybe (FingerTree a)
-- tail' t = case viewl t of (Just (_,rest)) -> Just rest
--                           otherwise -> Nothing
--
--
-- instance IsList (FingerTree a) where
--   type Item (FingerTree a) = a
--   toList tree = case viewl tree of Nothing -> []
--                                    Just (x, rest) -> x:(toList rest)
--   fromList xs = foldr (<|) Empty xs
--
--
-- -- concat :: FingerTree a -> FingerTree a
-- infixl 4 ><
-- (><) :: FingerTree a -> FingerTree a -> FingerTree a
-- (><) left Empty = left
-- (><) left right = left |> hr >< tr
--   where Just (hr,tr) = viewl right
