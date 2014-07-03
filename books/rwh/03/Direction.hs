import Data.List (minimumBy, sortBy)
import Data.Function (on)
 
data Point = Point {x :: Double, y :: Double}
type Points = [Point]
data Direction = ToLeft | ToRight | Straight deriving (Eq, Show)
type Directions = [Direction]

direction :: Point -> Point -> Point -> Direction
direction a b c 
  | k > 0 = ToLeft
  | k < 0 = ToRight
  | otherwise = Straight
  where
    (vx, vy)  = (x b - x a, y b - y a)
    (wx, wy)  = (x c - x b, y c - y b)
    k = vx * wy - vy * wx

directions :: Points -> Directions
directions (a:xs@(b:c:_)) = direction a b c : directions xs
directions _              = []

grahamscan :: Points -> Points
grahamscan xs = undefined
  where
    p           = minimumBy (compare `on` y) xs
    sort        = sortBy (angle p)
    angle       = undefined
