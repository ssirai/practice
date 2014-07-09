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

indexOfCoord :: World -> Index -> Coord
indexOfCoord world i =
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
