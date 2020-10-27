module HW5 where

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)
data Rose a = Node a [Rose a] deriving (Show, Eq)


--BINARY TREES
mirror :: Tree a -> Tree b -> Bool
mirror Tip Tip = True
mirror (Bin outsideL mid1 insideR) (Bin insideL mid2 outsideR) = (mirror outsideL outsideR) && (mirror insideR insideL)
mirror _ _ = False
--(a)
fromTree :: Tree a -> [a]
fromTree Tip = []
fromTree (Bin l x r) = ((fromTree l) ++ [x] ++ (fromTree r))
--(b)
trunc :: Int -> Tree a -> Tree a
trunc 0 _ = Tip
trunc d (Bin l x r) | d <= 0 = Tip
                    | otherwise = Bin (trunc (d-1) l) x (trunc (d-1) r)
--(c)
symmetric :: (Eq a) => Tree a -> Bool
symmetric t = mirror t t


--ROSE TREES
--(a)
sumRose :: (Num a) => Rose a -> a
sumRose (Node x cs) = x + sum (map sumRose cs)
--(b)
maximumRose :: (Ord a) => Rose a -> a
maximumRose (Node x []) = x
maximumRose (Node x cs) = maximum ([x]++(map maximumRose cs))
--(c)
sizeRose :: Rose a -> Int
sizeRose (Node _ []) = 1
sizeRose (Node _ cs) = 1 + sum (map sizeRose cs)
--(d)
fanout :: Rose a -> Int
fanout (Node _ []) = 0
fanout (Node _ cs) = maximum ([length cs] ++ (map fanout cs))


--ISOMORPHISM
foldRose :: (a -> [b] -> b) -> Rose a -> b
foldRose f (Node x cs) = f x (map (foldRose f) cs)

--(a)
toRoses :: Tree a -> [Rose a]
toRoses Tip = []
toRoses (Bin l x r) = (Node x (toRoses l)):(toRoses r) 

--(b)
fromRoses :: [Rose a] -> Tree a
fromRoses [] = Tip
fromRoses ((Node x cs):xs) = Bin (fromRoses cs) x (fromRoses xs)


{-
Test Cases:
sumRose (Node 2 [Node 3 [], Node 5 [Node 3 []], Node 4 []])

maximumRose (Node 2 [Node 3 [], Node 5 [Node 3 [Node 8 [Node 0 []]]], Node 4 []])
maximumRose (Node 2 [Node 3 [], Node 5 [Node 3 [Node 0 [Node 0 []]]], Node 4 []])
maximumRose (Node 2 [Node 3 [], Node 5 [Node 1 [Node 0 [Node 0 []]]], Node 4 []])
maximumRose (Node 2 [Node 3 [], Node 1 [Node 1 [Node 0 [Node 0 []]]], Node 4 []])

sizeRose (Node 2 [Node 3 [], Node 1 [Node 1 [Node 0 [Node 0 []]]], Node 4 []])
sizeRose (Node 2 [Node 3 [], Node 1 [Node 1 [Node 0 []]], Node 4 []])
sizeRose (Node 2 [Node 3 [], Node 1 [Node 1 [Node 0 []]]])

fanout (Node 2 [Node 3 [], Node 1 [Node 1 [Node 0 []]], Node 4 []])
fanout (Node 2 [Node 3 [], Node 1 [Node 1 [Node 0 [Node 0 [], Node 0 [], Node 0 [], Node 0 []]]], Node 4 []])
fanout (Node 0 [])
fanout (Node 0 [Node 0 []])
fanout (Node 0 [Node 0 [Node 0 [Node 0 [Node 0 [], Node 0 []]]]])

toRoses (Bin Tip 1 (Bin Tip 2 (Bin Tip 3 Tip)))
toRoses (Bin (Bin (Bin Tip 7 Tip) 6 Tip) 2 (Bin (Bin Tip 8 Tip) 3 (Bin (Bin (Bin Tip 9 (Bin Tip 10 (Bin Tip 11 Tip))) 5 Tip) 4 Tip)))
toRoses (Bin (Bin (Bin (Bin Tip 7 Tip) 6 Tip) 2 (Bin (Bin Tip 8 Tip) 3 (Bin (Bin (Bin Tip 9 (Bin Tip 10 (Bin Tip 11 Tip))) 5 Tip) 4 Tip))) 1 Tip)

fromRoses [Node 1 [], Node 2 [], Node 3 []]
fromRoses [Node 1 [Node 2 [Node 6 [Node 7 []]], Node 3 [Node 8 []], Node 4 [Node 5 [Node 9 [], Node 10 [], Node 11 []]]]]
fromRoses [Node 2 [Node 6 [Node 7 []]], Node 3 [Node 8 []], Node 4 [Node 5 [Node 9 [], Node 10 [], Node 11 []]]]
-}
