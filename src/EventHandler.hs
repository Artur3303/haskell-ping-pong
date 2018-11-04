module EventHandler(handleEvent) where

import State
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

handleEvent :: Event -> State -> State
handleEvent (EventKey (Char       'w')     Down _ _) gameState = gameState { keyWisPressed = True }
handleEvent (EventKey (Char       'w')     Up   _ _) gameState = gameState { keyWisPressed = False }
handleEvent (EventKey (Char       's')     Down _ _) gameState = gameState { keySisPressed = True }
handleEvent (EventKey (Char       's')     Up   _ _) gameState = gameState { keySisPressed = False }
handleEvent (EventKey (SpecialKey KeyUp)   Down _ _) gameState = gameState { keyUpIsPressed = True }
handleEvent (EventKey (SpecialKey KeyUp)   Up   _ _) gameState = gameState { keyUpIsPressed = False }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) gameState = gameState { keyDownIsPressed = True }
handleEvent (EventKey (SpecialKey KeyDown) Up   _ _) gameState = gameState { keyDownIsPressed = False }
handleEvent _                                        gameState = gameState