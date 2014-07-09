{-# LANGUAGE TemplateHaskell #-}
module World where
import Cell
import System.Random
import Control.Monad
import Control.Lens hiding (Index)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import qualified Data.Vector as Vec

type Vec = Vec.Vector
type Index = Int
type Coord = (Int, Int)

data World = World
            { _cells            :: Vec Cell
            , _width            :: Int
            , _height           :: Int
            , _cellSize         :: Int
            , _cellSpace        :: Int
            , _cellOldAge       :: Int
            , _simulationPeriod :: Float
            , _elapsedTime      :: Float
            }
 
makeLenses ''World

indexOfCoord :: World -> Coord -> Index
indexOfCoord world (x, y) = x + y * (world^.width)

coordOfIndex :: World -> Index -> Coord
coordOfIndex world i =
  (i `mod` world^.width, i `mod` world^.height)

randomWorld :: (Int, Int) -> IO World
randomWorld (width, height) = do
  bools <- replicateM (width * height) randomIO
  return $ World {
              _cells            = Vec.fromList $ map cellOfBool bools
            , _width            = width
            , _height           = height
            , _cellSize         = 5
            , _cellSpace        = 1
            , _cellOldAge       = 20
            , _simulationPeriod = 0.1
            , _elapsedTime      = 0
            }

cellOfBool :: Bool -> Cell
cellOfBool b = case b of
                 True   -> CellAlive 0
                 False  -> CellDead

getCell :: World -> Coord -> Cell
getCell world coord@(x, y)
  | x < 0 || x >= world^.width = CellDead
  | y < 0 || y >= world^.height = CellDead
  | otherwise = (world^.cells) Vec.! indexOfCoord world coord

getNeighbourhood :: World -> Coord -> [Cell]
getNeighbourhood world (ix, iy) =
  let indexes = [(x, y) | x <- [ix - 1 .. ix + 1],
                          y <- [iy - 1 .. iy +1],
                          not (x == ix && y == iy)]
  in map (getCell world) indexes

stepCell :: Cell -> [Cell] -> Cell
stepCell cell neighbours =
  let live = length $ filter isAlive neighbours
  in case cell of
        CellAlive age -> if elem live [2, 3] then CellAlive (age + 1) else CellDead
        CellDead      -> if live == 3        then CellAlive 0         else CellDead

stepIndex :: World -> Int -> Cell -> Cell
stepIndex world index cell =
  let coord = coordOfIndex world index
      neigh = getNeighbourhood world coord
  in stepCell cell neigh

stepWorld :: World -> World
stepWorld world = cells .~ vs $ world
  where
    vs = Vec.imap (stepIndex world) (world^.cells)

simulateWorld :: ViewPort -> Float -> World -> World
simulateWorld _ time world
  | world^.elapsedTime >= world^.simulationPeriod =
      let world' = stepWorld world in elapsedTime .~ 0 $ world'
  | otherwise =
      elapsedTime .~ time + world^.elapsedTime $ world

