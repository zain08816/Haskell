
-- Problem 1: 
median :: Integer -> Integer -> Integer -> Integer

median a b c | ((a > b) /= (a > c)) = a 
             | ((b > a) /= (b > c)) = b
             | otherwise = c
--main = putStrLn (show (median 1 3 2))

-- Problem 2: 
fib :: Integer -> Integer

fib n | n < 0 = 0
      | n == 0 = 1
      | n == 1 = 1
      | otherwise = fib (n - 1) + fib (n - 2)
main = putStrLn $ show $ fib (4)

-- Problem 3: 
fibs :: Integer -> [Integer]

fibs n = map fib [0..(n)]

--main = putStrLn $ show $  fibs (0)