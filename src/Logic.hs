module Logic where

import EventHandler
import State

updateGameState :: Float -> State -> State
updateGameState secs = updateTime secs . updateBallPos . keyW . keyS . keyUp . keyDown where
    keyW    gameState = if keyWisPressed    gameState then moveLeftPlatform  6.0    gameState else gameState
    keyS    gameState = if keySisPressed    gameState then moveLeftPlatform  (-6.0) gameState else gameState
    keyUp   gameState = if keyUpIsPressed   gameState then moveRightPlatform 6.0    gameState else gameState
    keyDown gameState = if keyDownIsPressed gameState then moveRightPlatform (-6.0) gameState else gameState

updateTime :: Float -> State -> State
updateTime secs gameState = gameState { time = secs }

calcNewY :: Float -> Float -> Float
calcNewY oldY delta = if newY <= maxY && newY >= minY then newY
                      else oldY
                        where newY = oldY + delta

moveLeftPlatform :: Float -> State -> State
moveLeftPlatform delta gameState = gameState
    { leftPlatformPos = ( fst pos, calcNewY (snd pos) delta ) } where
        pos = leftPlatformPos gameState

moveRightPlatform :: Float -> State -> State
moveRightPlatform delta gameState = gameState
    { rightPlatformPos = ( fst pos, calcNewY (snd pos) delta ) } where
        pos = rightPlatformPos gameState

updateBallPos :: State -> State
updateBallPos = moveBall . rebound

rebound :: State -> State
rebound gameState = if aroundLeft || aroundRight then recalcSpeed gameState else gameState where
    posX           = fst $ ballPos gameState
    posY           = snd $ ballPos gameState
    leftPlatformY  = snd $ leftPlatformPos  gameState
    rightPlatformY = snd $ rightPlatformPos gameState
    aroundLeft     = (posX>=(-360.0) && posX<=(-350.0)) && (posY<=leftPlatformY && posY>=leftPlatformY - 60)
    aroundRight    = (posX>=350.0 && posX<=360.0) && (posY<=rightPlatformY && posY>=rightPlatformY - 60)


recalcSpeed :: State -> State
recalcSpeed gameState = gameState { ballSpeed = newSpeed } where
    oldSpeed = ballSpeed gameState
    newSpeed = (negate $ fst oldSpeed, snd oldSpeed)

moveBall :: State -> State
moveBall gameState = gameState { ballPos = newPos } where
    speedX = fst $ ballSpeed gameState
    speedY = snd $ ballSpeed gameState
    oldX   = fst $ ballPos   gameState
    oldY   = snd $ ballPos   gameState
    newPos = (oldX + speedX,oldY + speedY)


maxY :: Float
maxY = 200.0

minY :: Float
minY = -240.0