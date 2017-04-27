data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

data Direction = LeftTurn | RightTurn | Straight deriving (Show, Eq)

magZCross (Cartesian2D x1 y1) (Cartesian2D x2 y2) (Cartesian2D x3 y3) =
    (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

turn p1 p2 p3
        | magZ < 0 = RightTurn
        | magZ > 0 = LeftTurn
        | otherwise = Straight
    where magZ = magZCross p1 p2 p3

turns (p1:p2:[]) = []
turns (p1:p2:p3:ps) = (turn p1 p2 p3):(turns (p2:p3:ps))
turns _ = []
