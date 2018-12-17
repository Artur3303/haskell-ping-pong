module Main(main) where

import Graphics.Gloss
import State
import EventHandler
import Renderer
import Logic
import System.Random

window :: Display
window = InWindow "Ping Pong Game" (900,700) (500,150)

background :: Color
background = white

fps :: Int
fps = 60

main :: IO ()
main = do
  g <- newStdGen
  let iState = initialGameState { gen = g }
  play window background fps iState render handleEvent updateGameState
   where
        fps = 60
