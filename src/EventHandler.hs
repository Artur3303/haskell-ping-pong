module EventHandler(handleEvent) where

import State
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

handleEvent :: Event -> State -> State
handleEvent (EventKey (SpecialKey KeyUp)   Down) _ _) gameState = moveFstPlatform 10    gameState
handleEvent (EventKey (SpecialKey KeyDown) Down) _ _) gameState = moveFstPlatform (-10) gameState


moveFstPlatform :: Int -> State -> State