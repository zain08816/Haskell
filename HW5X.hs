{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}

module HW5X where

import HW5

data GRose c a = GNode a (c (GRose c a))

deriving instance (Show a, Show (c (GRose c a))) => Show (GRose c a)
deriving instance (Eq a, Eq (c (GRose c a))) => Eq (GRose c a)

mapGRoseList f (GNode x cs) =
        GNode (f x) (map (mapGRoseList f) cs)

--EXTRA CREDIT

--(a)
genRose :: Rose a -> GRose [] a
genRose (Node x []) = GNode x []
genRose (Node x cs) = GNode x (map genRose cs)

ungenRose :: GRose [] a -> Rose a
ungenRose (GNode x []) = Node x []
ungenRose (GNode x cs) = Node x (map ungenRose cs)


--(b)
-- data TwoThree a = Zero 
--                 | Two (GRose TwoThree a) (GRose TwoThree a)
--                 | Three (GRose TwoThree a) (GRose TwoThree a) (GRose TwoThree a) deriving (Show, Eq)

data TwoThree a = Zero a | Two a (TwoThree a) (TwoThree a) | Three a (TwoThree a) (TwoThree a) (TwoThree a) deriving (Show, Eq)

-- data TwoThree a = Zero | Two (GRose TwoThree a) (GRose TwoThree a) | Three (GRose TwoThree a) (GRose TwoThree a) (GRose TwoThree a) deriving (Show, Eq)



--(c)
sum23_tree :: (Num a) => TwoThree a -> a
sum23_tree (Zero y) = y
sum23_tree (Two y one two) = y + (sum23_tree one) + (sum23_tree two)
sum23_tree (Three y one two three) = y + (sum23_tree one) + (sum23_tree two) + (sum23_tree three)


toNum :: TwoThree a -> a
toNum (Zero x) = x
toNum (Two x _ _) = x
toNum (Three x _ _ _) = x

-- sum23 :: (Num (GRose TwoThree a), Num a) => (GRose TwoThree a) -> a
-- sum23 (GNode x (Zero y)) = x 
-- sum23 (GNode x (Two y (one) (two))) = x 
-- sum23 (GNode x (Three y (one) (two) (three))) = x 


{-
Test Cases 
genRose (Node 1 [Node 2 [Node 6 [Node 7 []]], Node 3 [Node 8 []], Node 4 [Node 5 [Node 9 [], Node 10 [], Node 11 []]]])
ungenRose (GNode 1 [GNode 2 [GNode 6 [GNode 7 []]],GNode 3 [GNode 8 []],GNode 4 [GNode 5 [GNode 9 [],GNode 10 [],GNode 11 []]]])
-}