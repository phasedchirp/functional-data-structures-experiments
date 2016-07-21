{-# LANGUAGE OverloadedLists, TypeFamilies #-}
import GHC.Exts (IsList(..))

-- data Tree a = Leaf a | Succ (Tree (Node a)) deriving (Show)


-- x = Node3 'a' 'b' 'c'
-- y = Succ $ Leaf (Node2 (Succ (Leaf x)) (Succ (Leaf x)))


data FingerTree a = Empty | Single a | Deep (Digit a) (FingerTree (Node a)) (Digit a) deriving (Show)
data Node a = Node2 a a | Node3 a a a deriving (Show)
type Digit a = [a]

infixr 5 <|
(<|) :: a -> FingerTree a -> FingerTree a
(<|) x Empty = Single x
(<|) x (Single y) = Deep [x] Empty [y]
(<|) x (Deep [a,b,c,d] m rf) = Deep [x, a] ((Node3 b c d) <| m) rf
(<|) x (Deep lf m rf) = Deep (x:lf) m rf

infixl 6 |>
(|>) :: FingerTree a -> a -> FingerTree a
(|>) Empty x = Single x
(|>) (Single y) x =  Deep [y] Empty [x]
(|>) (Deep lf m [a,b,c,d]) x = Deep lf (m |> (Node3 a b c) ) [d,x]
(|>) (Deep lf m rf) x = Deep lf m (rf ++ [x])

viewl :: FingerTree a -> Maybe (a, FingerTree a)
viewl Empty = Nothing
viewl (Single x) = Just (x, Empty)
viewl (Deep [x] m rf) = case viewl m of Just ((Node2 a b) , deeper) -> Just (x, Deep [a,b] deeper rf)
                                        Just ((Node3 a b c), deeper) -> Just (x, Deep [a,b,c] deeper rf)
                                        Nothing -> case rf of [a] -> Just (x, Single a)
                                                              [a,b] -> Just (x, Deep [a] Empty [b])
                                                              [a,b,c] -> Just (x, Deep [a,b] Empty [c])
                                                              [a,b,c,d] -> Just (x, Deep [a,b] Empty [c,d])
viewl (Deep lf m rf) = Just (head lf, Deep (tail lf) m rf)

head' :: FingerTree a -> Maybe a
head' t = case viewl t of (Just (x,_)) -> Just x
                          otherwise -> Nothing

tail' :: FingerTree a -> Maybe (FingerTree a)
tail' t = case viewl t of (Just (_,rest)) -> Just rest
                          otherwise -> Nothing


instance IsList (FingerTree a) where
  type Item (FingerTree a) = a
  toList tree = case viewl tree of Nothing -> []
                                   Just (x, rest) -> x:(toList rest)
  fromList xs = foldr (<|) Empty xs


-- concat :: FingerTree a -> FingerTree a
infixl 4 ><
(><) :: FingerTree a -> FingerTree a -> FingerTree a
(><) left Empty = left
(><) left right = left |> hr >< tr
  where Just (hr,tr) = viewl right
