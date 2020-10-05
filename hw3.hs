-- Problem 1
newtype Poly = Poly [Double] deriving (Show, Eq)


-- (a)
coeffs :: [Double] -> Poly

{-
This function will take in a [Double] and return a Poly
As per instructions, Poly cannot have 0 as the last element.
To maintain this, we recursivly call coeffs until there is no trailing 0s
If the the list given is null or only contains only 0, the Poly is []
-}
coeffs nums | null nums = Poly []
            | nums == [0] = Poly []
            | last nums == 0 = coeffs $ take ((length nums) - 1) nums
            | otherwise = Poly nums

-- main = putStrLn $ show $ coeffs [0, 0, 0]

-- (b)
at :: Poly -> Double -> Double

{-
In order to evaluate the polynomial, we need to evalute 
each monomial. To do this, we map the num to each exponent to the number given. 
This will solve num^x. We then need to multiply each by the coefficent. 
After that, we then will have [c*num^x]. We will now add them all 
together using sum for the final answer.
-}
at (Poly nums) num = sum $ zipWith (*) nums (map (num**) [0..])

-- main = putStrLn $ show $ (coeffs [1, 1, 0, 1]) `at` 3

-- (c)
add :: Poly -> Poly -> Poly

{-
This function will add 2 polynomials together.
To do this, we will essentially just be dealing with the coefficients/
We will zipWith to add them together, but this wont work for Polys of differnt
lengths. To handle this, we adding trailing zeros to the end of the shorter array.
-}

add (Poly nums1) (Poly nums2) | length nums1 > length nums2 = coeffs $ (zipWith (+) nums1 (nums2 ++ [0,0..])) 
                              | length nums1 < length nums2 = coeffs $ (zipWith (+) nums2 (nums1 ++ [0,0..])) 
                              | otherwise = coeffs $ (zipWith (+) nums1 nums2)


main = putStrLn $ show $ add (Poly [1,0,1, 35, 20, 0, 1]) (Poly [0,1,-1, 1])
