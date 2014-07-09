module Main where

import World
import Cell
import Graphics.Gloss
import qualified Data.Vector as Vec
import Control.Lens hiding (Index)

main :: IO ()
main = do
  let width = 150
  let height = 100
  world <- randomWorld (width, height)
  simulate (InWindow "John Conway's Game of Life"
                     (windowSizeOfWorld world) (5, 5))
            white 10 world drawWorld simulateWorld

drawWorld :: World -> Picture
drawWorld world =
  let (windowWidth, windowHeight) = windowSizeOfWorld world
      offsetX = - fromIntegral windowWidth / 2
      offsetY = - fromIntegral windowHeight / 2
  in translate offsetX offsetY
        $ Pictures
        $ Vec.toList
        $ Vec.imap (drawCell world) (world^.cells)

drawCell :: World -> Index -> Cell -> Picture
drawCell world index cell =
  let cs      = fromIntegral (world^.cellSize)
      cp      = fromIntegral (world^.cellSpace)
      (x, y)  = coordOfIndex world index
      fx      = fromIntegral x * (cs + cp) + 1
      fy      = fromIntegral y * (cs + cp) + 1
  in pictureOfCell (world^.cellOldAge)
                   (world^.cellSize)
                   fx
                   fy
                   cell

windowSizeOfWorld :: World -> (Int, Int)
windowSizeOfWorld world =
  let size          = world^.cellSize
      space         = world^.cellSpace
      cellPad       = size + space
      width'        = cellPad * (world^.width ) + space
      height'       = cellPad * (world^.height) + space
  in (width', height')
