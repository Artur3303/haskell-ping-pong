module EventHandler(handleEvent) where

import State
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Renderer
import System.Random

startNewGame :: State -> State
startNewGame gameState = 
    let (one, g1) = random $ gen gameState
        (two, newgen) = random g1
    in  gameState { ballPos = (0, 0), ballSpeed = (600 * (one-0.5), 400 * (two - 0.5)), suspended = False, gen = newgen } 

handleEvent :: Event -> State -> State
handleEvent (EventKey (Char       'r')     Up   _ _) gameState = if suspended gameState then startNewGame gameState else gameState
handleEvent (EventKey (Char       'w')     Down _ _) gameState = gameState { keyWisPressed    = True }
handleEvent (EventKey (Char       'w')     Up   _ _) gameState = gameState { keyWisPressed    = False }
handleEvent (EventKey (Char       's')     Down _ _) gameState = gameState { keySisPressed    = True }
handleEvent (EventKey (Char       's')     Up   _ _) gameState = gameState { keySisPressed    = False }
handleEvent (EventKey (SpecialKey KeyUp)   Down _ _) gameState = gameState { keyUpIsPressed   = True }
handleEvent (EventKey (SpecialKey KeyUp)   Up   _ _) gameState = gameState { keyUpIsPressed   = False }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) gameState = gameState { keyDownIsPressed = True }
handleEvent (EventKey (SpecialKey KeyDown) Up   _ _) gameState = gameState { keyDownIsPressed = False }
handleEvent _                                        gameState = gameState