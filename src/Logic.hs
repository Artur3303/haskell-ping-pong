module Logic where

import EventHandler
import State

updateGameState :: Float -> State -> State
updateGameState secs = updateTime secs . keyW . keyS . keyUp . keyDown where
    keyW    gameState = if keyWisPressed    gameState then moveFstPlatform 10.0    gameState else gameState
    keyS    gameState = if keySisPressed    gameState then moveFstPlatform (-10.0) gameState else gameState
    keyUp   gameState = if keyUpIsPressed   gameState then moveSndPlatform (10.0)  gameState else gameState
    keyDown gameState = if keyDownIsPressed gameState then moveSndPlatform (-10.0) gameState else gameState

updateTime :: Float -> State -> State
updateTime secs gameState = gameState { time = secs }

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

maxY :: Float
maxY = 200.0

minY :: Float
minY = -240.0