module Cell where
import Graphics.Gloss

data Cell = CellAlive Int
          | CellDead
          deriving (Show, Eq)

isAlive :: Cell -> Bool
isAlive cell = case cell of
                CellAlive _ -> True
                CellDead    -> False

cellShape :: Int -> Int -> Int -> Picture
cellShape cellSize posXi posYi = 
  let cs      = fromIntegral cellSize
      posX    = fromIntegral posXi
      posY    = fromIntegral posYi
      x1      = posX
      x2      = posX + cs
      y1      = posY
      y2      = posY + cs
  in Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

pictureOfCell :: Int -> Int -> Int -> Int -> Cell -> Picture
pictureOfCell oldAge cellSize posX posY cell =
  case cell of
    CellAlive age -> Color (ageColor oldAge age)	(cellShape cellSize posX posY)
    CellDead      -> Color (greyN 0.8) (cellShape cellSize posX posY)

ageColor :: Int -> Int -> Color
ageColor oldAge age =
  let (r, g, b) = rampColorHotToCold 0 (fromIntegral oldAge) (fromIntegral age)
  in makeColor r g b 1.0

rampColorHotToCold :: (Ord a, Floating a) => a -> a -> a -> (a, a, a)
rampColorHotToCold vmin vmax vNotNorm =
  let v   | vNotNorm < vmin   = vmin
          | vNotNorm > vmax   = vmax
          | otherwise         = vNotNorm
      dv = vmax - vmin
      result | v < vmin + 0.25 * dv = (0, 4 * (v - vmin) / dv, 1.0)
             | v < vmin + 0.5 * dv  = (0, 1.0, 1 + 4 * (vmin + 0.25 * dv - v) / dv)
             | v < vmin + 0.75 * dv = (4 * (v - vmin - 0.5 * dv) / dv, 1.0, 0.0)
             | otherwise            = (1.0, 1 + 4 * (vmin + 0.75 * dv - v) / dv, 0)
  in result

