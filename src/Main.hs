module Main(main) where

import Graphics.Gloss
import State
import EventHandler
import Renderer

window :: Display
window = InWindow "Ping Pong Game" (900,800) (500,200)

background :: Color
background = white

fps :: Int
fps = 60

main :: IO ()
main = do
  play window background initialGameState render handleEvent
