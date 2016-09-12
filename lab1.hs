-- Lab 1

-- Assignment 1
-- power n k uses k+1 computing steps

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Assignment 2

power1 :: Integer -> Integer -> Integer
power1 n k | k<0    = error "Function not defined for negative k"
           | k == 0 = 1
           | k>0    = product(replicate (fromIntegral k) n)

-- Assignment 3

power2 :: Integer -> Integer -> Integer
power2 n k | k<0       = error "Function not defined for negative k"
           | k == 0    = 1
           | even k    = power2 (n * n) (k `div` 2)
           | otherwise = n * power2 n (k-1)

-- Assignment 4

comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = power n k == power1 n k

comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = power1 n k == power2 n k

-- testing, just for fun!

-- calculating 10^100000 using power, took 10.98 seconds (2 301 438 176 bytes)
-- calculating 10^100000 using power1, took 10.84 seconds (2 282 879 776 bytes)
-- calculating 10^100000 using power2, took 2.42 seconds (80 968 464 bytes)

test :: [Bool]
test = [comparePower1 n k | n <- [(-4),0,1,9,2678], k <- [0,1,6,1954]]
        ++ [comparePower2 n k | n <- [(-4),0,1,9,2678], k <- [0,1,6,1954]]

hej = 1
