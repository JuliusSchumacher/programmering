

average :: Double -> Double -> Double
average x y = (x+y)/2

average2 :: Double -> Double -> Double -> Double
average2 x y z = (x+y+z)/3

leapyear :: Integer -> Integer
leapyear x| mod x 4 == 0 = 366
          | otherwise = 365