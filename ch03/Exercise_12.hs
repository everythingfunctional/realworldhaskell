import qualified Data.List as DL

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

lowest_y (Cartesian2D x1 y1) (Cartesian2D x2 y2)
    | y1 == y2 = compare x1 x2
    | otherwise = compare y1 y2

dotProduct (Cartesian2D x1 y1) (Cartesian2D x2 y2) = x1 * x2 + y1 * y2

magnitude (Cartesian2D x y) = sqrt (x^2 + y^2)

cosAngleWith (Cartesian2D x1 y1) (Cartesian2D x2 y2)
    = let vector = Cartesian2D (x2 - x1) (y2 - y1)
          in (dotProduct vector (Cartesian2D 1 0)) / (magnitude vector)

distance (Cartesian2D x1 y1) (Cartesian2D x2 y2) = magnitude (Cartesian2D (x2 - x1) (y2 - y1))

sortByAngle p ps = DL.sortBy angleComparison ps where
    angleComparison p2 p1
        | angle_compare == EQ = compare (distance p p1) (distance p p2)
        | otherwise = angle_compare
        where angle_compare = compare (cosAngleWith p p1) (cosAngleWith p p2)

grahamScan points = let (p:ps) = DL.sortBy lowest_y points
                        ordered = sortByAngle p ps
                        (p1:p2:rest) = ordered
                        first_three = [p2,p1,p]
                        in scan first_three rest

scan hull [] = removeRightTurns hull
scan hull (r:rs) = scan (removeRightTurns (r:hull)) rs

removeRightTurns points@(p1:p2:p3:ps)
    | turn p3 p2 p1 == RightTurn = removeRightTurns (p1:p3:ps)
    | otherwise = points

x1 = Cartesian2D 4 6
x2 = Cartesian2D 7 7
x3 = Cartesian2D 7 3
x4 = Cartesian2D 5 4
x5 = Cartesian2D 2 7
x6 = Cartesian2D 7 10
x7 = Cartesian2D 5 8
x8 = Cartesian2D 10 6
x9 = Cartesian2D 5 1
x10 = Cartesian2D 2 2
x11 = Cartesian2D 2 1

points = [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11]
