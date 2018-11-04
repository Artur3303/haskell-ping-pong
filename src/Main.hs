module Main(main) where

import Graphics.Gloss
import State
import EventHandler
import Renderer
import Logic

window :: Display
window = InWindow "Ping Pong Game" (900,700) (500,150)

background :: Color
background = white

fps :: Int
fps = 60

main :: IO ()
main = do
  play window background fps initialGameState render handleEvent updateGameState
