{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}

module Main where

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad.State (State, execState, get)
import Control.Monad (when)

import Data.Set (Set, empty)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import System.Random (randomRs, newStdGen)

gameSize        = 300
windowWidth     = 800
windowHeight    = 600
ballRadius      = 0.02
speedIncrease   = 1.2
losingAccuracy  = 0.9
winningAccuracy = 0.1
initialSpeed    = 0.6
paddleWidth     = 0.02
paddleHeight    = 0.3
paddleSpeed     = 1
textSize        = 0.001

data Pong = Pong
  { _ballPos    :: Point
  , _ballSpeed  :: Vector
  , _paddle1    :: Float
  , _paddle2    :: Float
  , _score      :: (Int, Int)
  , _vectors    :: [Vector]
  , _keys       :: Set Key
  }

makeLenses ''Pong

_x = _1
_y = _2

initial :: Pong
initial = Pong (0, 0) (0, 0) 0 0 (0, 0) [] empty

hitPos :: Point -> Vector -> Float
hitPos (x, y) (u, v) = ypos
  where
    xdist = if u >= 0 then 1 - x else 3 + x
    time  = xdist / abs u
    ydist = v * time
    ypos  = bounce (y + ydist)
    o     = 1 - ballRadius
    bounce n
      | n > o     = bounce (2 * o - n)
      | n < -o    = bounce ((-2) * o - n)
      | otherwise = n

accuracy :: Pong -> Float
accuracy p = g . f . fromIntegral $ p^.score._1 - p^.score._2
  where
    f x = (0.14 * x - 10) + 0.5
    g = min losingAccuracy . max winningAccuracy

update :: Float -> Pong -> Pong
update time = execState $ do
  updatePaddles time
  updateBall time
  checkBounds

updateBall :: Float -> State Pong ()
updateBall time = do
  (u, v) <- use ballSpeed
  ballPos += (time *u, time * v)
  ballPos.both %= clamp ballRadius

updatePaddles :: Float -> State Pong ()
updatePaddles time = do
  p <- get
  let paddleMovement = time * paddleSpeed
      keyPressed key = p^.keys.contains (SpecialKey key)

  when (keyPressed KeyUp)   $ paddle1 += paddleMovement
  when (keyPressed KeyDown) $ paddle1 -= paddleMovement

  let optimal = hitPos (p^.ballPos) (p^.ballSpeed)
      acc     = accuracy p
      target  = optimal * acc + (p^.ballPos._y) * (1 - acc)
      dist    = target - p^.paddle2

  when (abs dist > paddleHeight / 3) $
    case compare dist 0 of
      GT  -> paddle2 += paddleMovement
      LT  -> paddle2 -= paddleMovement
      _   -> return ()

  paddle1 %= clamp (paddleHeight/2)
  paddle2 %= clamp (paddleHeight/2)

clamp :: Float -> Float -> Float
clamp pad = max (pad - 1) . min (1 - pad)

checkBounds :: State Pong ()
checkBounds = do
  p <- get
  let (x, y) = p^.ballPos

  when (abs y >= edge) $
    ballSpeed._y %= negate

  let check paddle other
        | y >= p^.paddle - paddleHeight / 2 && y <= p^.paddle + paddleHeight / 2 = do
            ballSpeed._x    %= negate
            ballSpeed._y    += 3 * (y - p^.paddle)
            ballSpeed .both *= speedIncrease
        | otherwise = do
            score.other += 1
            reset
  when (x >= edge)  $ check paddle2 _1
  when (x <= -edge) $ check paddle1 _2
  where
    edge = 1 - ballRadius

reset :: State Pong ()
reset = do
  ballPos .= (0, 0)
  ballSpeed <~ nextSpeed

nextSpeed :: State Pong Vector
nextSpeed = do
  v:vs <- use vectors
  vectors .= vs
  return v

draw :: Pong -> Picture
draw p = scale gameSize gameSize $ Pictures
  [ drawBall    `at` p^.ballPos
  , drawPaddle  `at` (-paddleX, p^.paddle1)
  , drawPaddle  `at` ( paddleX, p^.paddle2)
  , drawScore (p^.score) `at` (-0.1, 0.85)
  , rectangleWire 2 2
  ]
    where
      paddleX = 1 + paddleWidth / 2
      p `at` (x, y) = translate x y p; infixr 1 `at`

drawPaddle :: Picture
drawPaddle = rectangleSolid paddleWidth paddleHeight

drawBall :: Picture
drawBall = circleSolid ballRadius

drawScore :: (Int, Int) -> Picture
drawScore (x, y) = scale textSize textSize . text $ show x ++ " " ++ show y

handle :: Event -> Pong -> Pong
handle (EventKey k s _ _) = keys.contains k .~ (s == Down)
handle _ = id

main :: IO ()
main = do
  v:vs <- startingSpeeds
  let world = ballSpeed .~ v $ vectors .~ vs $ initial
  play display backColor fps world draw handle update
  where
    display = InWindow "Pong!" (windowWidth, windowHeight) (200, 200)
    backColor = white
    fps = 60

startingSpeeds :: IO [Vector]
startingSpeeds = do
  rs <- randomRs (-initialSpeed, initialSpeed) <$> newStdGen
  return . interleave $ filter ((> 0.2) . abs) rs
  where
    interleave :: [a] -> [(a,a)]
    interleave (x:y:xs) = (x,y) : interleave xs
    interleave _ = []
