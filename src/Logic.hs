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
rebound gameState = aroundWall $ aroundPlatform gameState where
    posX   = fst $ ballPos          gameState
    posY   = snd $ ballPos          gameState
    leftY  = snd $ leftPlatformPos  gameState
    rightY = snd $ rightPlatformPos gameState
    left   = (posX>=(-360.0) && posX<=(-355.0)) && (posY<=leftY  && posY>=leftY-60.0)
    right  = (posX>=355.0    && posX<=360.0)    && (posY<=rightY && posY>=rightY-60.0)
    wall   = (posY<=190.0 && posY>=180.0) || (posY<=(-280.0) && posY>=(-290.0))
    aroundWall gameState = if wall then
        gameState { ballSpeed = (fst $ ballSpeed gameState, negate $ snd $ ballSpeed gameState) }
        else gameState
    aroundPlatform gameState = if left || right then
        gameState { ballSpeed = (negate $ fst $ ballSpeed gameState, snd $ ballSpeed gameState) }
        else gameState

moveBall :: State -> State
moveBall gameState = let
    posX = fst $ ballPos gameState
    posY = snd $ ballPos gameState
    speedX = fst $ ballSpeed gameState
    speedY = snd $ ballSpeed gameState
    newBallPos = (posX + speedX, posY + speedY)
    in gameState { ballPos = newBallPos }

maxY :: Float
maxY = 200.0

minY :: Float
minY = -240.0