-- Part 1
data Poly = Poly [Double] deriving (Show, Eq)

coeffs :: [Double] -> Poly
coeffs nums | null nums = Poly []
            | nums == [0] = Poly []
            | last nums == 0 = coeffs $ take ((length nums) - 1) nums
            | otherwise = Poly nums


at :: Poly -> Double -> Double
at (Poly nums) num = sum $ zipWith (*) nums (map (num**) [0..])


add :: Poly -> Poly -> Poly
add (Poly nums1) (Poly nums2) | length nums1 > length nums2 = coeffs $ (zipWith (+) nums1 (nums2 ++ [0,0..])) 
                              | length nums1 < length nums2 = coeffs $ (zipWith (+) nums2 (nums1 ++ [0,0..])) 
                              | otherwise = coeffs $ (zipWith (+) nums1 nums2)

negateElement :: Double -> Double
negateElement num | num == 0 = 0
                  | otherwise = (num*(-1))

neg :: Poly -> Poly
neg (Poly nums) = coeffs $ (map negateElement nums)

toList :: Poly -> [Double]
toList (Poly nums) = nums

mult :: Poly -> Poly -> Poly
mult (Poly xs) _ | xs == [] = Poly [0]
mult (Poly (x:xs)) (Poly ys) = coeffs $ toList $ (add (Poly (map (*x) ys))  (Poly (0 : toList (mult (Poly xs) (Poly ys)))))

x :: Poly
x = Poly [0, 1]

instance Num Poly where
    (+) = add
    negate = neg
    (*) = mult
    fromInteger 0 = Poly []
    fromInteger n = Poly [fromInteger n]
    abs = error "No abs for Poly"
    signum = error "No signum for Poly"
