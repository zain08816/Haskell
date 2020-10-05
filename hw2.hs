
-- problem 1
data Color = Red | Green | Blue | Black deriving (Eq, Show)
data Width = Thin | Medium | Thick deriving (Eq, Show)
data Style = Solid | Dashed | Dotted | Wavy deriving (Eq, Show)

data Border =  Invisible | Visible Color Width Style deriving (Eq, Show)

-- (a)
thin_red :: Border 
thin_red = Visible Red Thin Wavy
--main = putStrLn $ show $ thin_red

-- (b)
green_wavy :: Border -> Bool
green_wavy Invisible = False
green_wavy (Visible color _ style) | color == Green && style == Wavy = True
                                   | otherwise = False



gw_line = Visible Green Medium Wavy
--main = putStrLn $ show $ green_wavy gw_line

-- problem 2
data Point = Point { x :: Double, y :: Double, z :: Double } deriving (Show, Eq)

-- (a)
dist :: Point -> Point -> Double

dist (Point x1 y1 z1) (Point x2 y2 z2) = sqrt $ ((x2 - x1)**2) + ((y2 - y1)**2) + ((z2 - z1)**2)

point1 :: Point
point1 = Point 7 4 3

point2 :: Point
point2 = Point 17 6 2

-- should return 10.246951
--main = putStrLn $ show $ dist point1 point2

-- (b)
projXY :: Point -> Point

projXY (Point x y _) = Point x y 0

-- sould return a point with x=7, y=4, z=0
--main = putStrLn $ show $ projXY point1