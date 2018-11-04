module EventHandler(handleEvent) where

import State
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

maxY :: Float
maxY = 200.0

minY :: Float
minY = -240.0

handleEvent :: Event -> State -> State
handleEvent (EventKey (Char       'w')     Down _ _) gameState = moveFstPlatform 20.0    gameState
handleEvent (EventKey (Char       's')     Down _ _) gameState = moveFstPlatform (-20.0) gameState
handleEvent (EventKey (SpecialKey KeyUp)   Down _ _) gameState = moveSndPlatform (20.0)  gameState
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) gameState = moveSndPlatform (-20.0) gameState
handleEvent _                                        gameState = gameState

calcNewY :: Float -> Float -> Float
calcNewY oldY delta = if newY <= maxY && newY >= minY then newY
                      else oldY
                        where newY = oldY + delta

moveFstPlatform :: Float -> State -> State
moveFstPlatform delta gameState = gameState
    { fstPlatformPos = ( fst pos, calcNewY (snd pos) delta ) } where
        pos = fstPlatformPos gameState

moveSndPlatform :: Float -> State -> State
moveSndPlatform delta gameState = gameState
    { sndPlatformPos = ( fst pos, calcNewY (snd pos) delta ) } where
        pos = sndPlatformPos gameState