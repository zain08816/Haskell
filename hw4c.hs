-- Part 1
data Poly a = Poly [a] deriving (Show, Eq)

coeffs :: (Eq a, Num a) => [a] -> Poly a
coeffs nums | null nums = Poly []
            | nums == [0] = Poly []
            | last nums == 0 = coeffs $ take ((length nums) - 1) nums
            | otherwise = Poly nums


at :: (Eq a, Num a, Floating a , Enum a) => Poly a -> a -> a
at (Poly nums) num = sum $ zipWith (*) nums (map (num**) [0..])

add :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
add (Poly nums1) (Poly nums2) | length nums1 > length nums2 = coeffs $ (zipWith (+) nums1 (nums2 ++ (repeat 0))) 
                              | length nums1 < length nums2 = coeffs $ (zipWith (+) nums2 (nums1 ++ (repeat 0))) 
                              | otherwise = coeffs $ (zipWith (+) nums1 nums2)

negateElement :: (Eq a, Num a) => a -> a
negateElement num | num == 0 = 0
                  | otherwise = (num*(-1))

neg :: (Eq a, Num a) => Poly a -> Poly a
neg (Poly nums) = coeffs $ (map negateElement nums)

toList :: Poly a -> [a]
toList (Poly nums) = nums

mult :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
mult (Poly xs) _ | xs == [] = Poly []
mult (Poly (x:xs)) (Poly ys) = coeffs $ toList $ (add (Poly (map (*x) ys))  (Poly (0 : toList (mult (Poly xs) (Poly ys)))))

x :: (Num a, Eq a) => Poly a
x = Poly [0, 1]

y :: (Num a, Eq a) => Poly (Poly a)
y = Poly [x]

f :: Poly (Poly Double)
f = Poly [(Poly [1, 2, 3])]

-- f `at` 0 :: Poly Double

-- f `at` 0 `at` 1 :: Double


instance (Num a, Eq a) => Num (Poly a) where
    (+) = add
    negate = neg
    (*) = mult
    fromInteger 0 = Poly []
    fromInteger n = Poly [fromInteger n]
    abs = error "No abs for Poly"
    signum = error "No signum for Poly"

