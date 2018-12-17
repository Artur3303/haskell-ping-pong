module Logic where

import EventHandler
import State
import Data.Maybe

updateGameState :: Float -> State -> State
updateGameState secs = updateTime secs . keyW . keyS . keyUp . keyDown . moveThings secs where
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

moveThings :: Float -> State -> State
moveThings secs = moveBall secs . moveLeftPlatform secs  .  moveRightPlatform secs . goal  . rebound               

moveLeftPlatform :: Float -> State -> State
moveLeftPlatform delta gameState = gameState
    { leftPlatformPos = ( fst pos, calcNewY (snd pos) delta ) } where
        pos = leftPlatformPos gameState

moveRightPlatform :: Float -> State -> State
moveRightPlatform delta gameState = gameState
    { rightPlatformPos = ( fst pos, calcNewY (snd pos) delta ) } where
        pos = rightPlatformPos gameState

-- updateBallPos :: State -> State
-- updateBallPos = moveBall . rebound . goal

goal :: State -> State
goal gameState = checkGoal gameState where
    posX       = fst $ ballPos gameState
    leftWall   = posX>=(-400.0) && posX<=(-370.0)
    rightWall  = posX>=370.0 && posX<=400.0
    leftScore  = fst $ score gameState
    rightScore = snd $ score gameState
    checkGoal gameState = 
        if leftWall then
            gameState { score     = (leftScore, rightScore + 1)
                      , ballPos   = ballPos initialGameState
                      , ballSpeed = (-200.0,50.0)
                      }
        else if rightWall then
            gameState { score     = (leftScore + 1, rightScore)
                      , ballPos   = ballPos initialGameState
                      , ballSpeed = (200.0,50.0)
                      }
        else gameState

rebound :: State -> State
rebound gameState = aroundWall $ aroundPlatform gameState where
    (vx, vy) = ballSpeed gameState
    (x, y)   = ballPos gameState
    leftY  = snd $ leftPlatformPos  gameState
    rightY = snd $ rightPlatformPos gameState
    left   = (x>=(-360.0) && x<=(-355.0)) && (y<=leftY  && y>=leftY-60.0)
    right  = (x>=355.0    && x<=360.0)    && (y<=rightY && y>=rightY-60.0)
    wall   = (y<=200.0 && y>=190.0) || (y<=(-290.0) && y>=(-300.0))
    archvy = abs vx * tan (normalAngle + archedAngle)
    normalAngle = atan (vy / abs vx)
    archedAngle = (y - 1.0) / (paddleLen * 0.7)
    aroundWall gameState = if wall then
        gameState {ballSpeed = (fst $ ballSpeed gameState, negate $ snd $ ballSpeed gameState)}
        else gameState
    aroundPlatform gameState = if left && (snd $ ballSpeed gameState) > 0 && keyWisPressed gameState then
        gameState { ballSpeed = (-vx, archvy)} 
        else if left && (snd $ ballSpeed gameState) > 0 && keySisPressed gameState then
            gameState { ballSpeed = (-vx, archvy)}
        else if left && (snd $ ballSpeed gameState) < 0 && keyWisPressed gameState then
    	    gameState { ballSpeed = (-vx, archvy)}
	    else if left && (snd $ ballSpeed gameState) < 0 && keySisPressed gameState then
            gameState { ballSpeed = (-vx, archvy)}
        else if right && (snd $ ballSpeed gameState) > 0 && keyUpIsPressed gameState then
            gameState { ballSpeed = (-vx, archvy)}
        else if right && (snd $ ballSpeed gameState) > 0 && keyDownIsPressed gameState then
            gameState { ballSpeed = (-vx, archvy)}
        else if right && (snd $ ballSpeed gameState) < 0 && keyUpIsPressed gameState then
            gameState {ballSpeed = (-vx, archvy)} 
        else if right && (snd $ ballSpeed gameState) < 0 && (keyDownIsPressed gameState) then
            gameState {ballSpeed = (-vx, archvy)}  
        else if left || right then 
            gameState { ballSpeed = (-vx, archvy) }
        else gameState

 

moveBall :: Float -> State -> State
moveBall secs gameState = if suspended gameState then gameState
                    else
                        gameState { ballPos = (x1, y2) }
                        where 
                          --old ball location and velocity
                          (x, y) = ballPos gameState
                          (vx, vy) = ballSpeed gameState

                          --new ball location
                          x1 = x + secs * vx
                          y2 = y + secs * vy

-- paddleBounce :: State -> State
-- paddleBounce gameState = gameState { ballSpeed = speed}
--     where 
--         (vx, vy) = ballSpeed gameState
--         (x, y)   = ballPos gameState
--         (hit, paddleY) = paddleCollision gameState
--         speed = if hit
--             then (-vx, archvy )
--             else (vx, vy)
--         archvy = abs vx * tan (normalAngle + archedAngle)
--         normalAngle = atan (vy / abs vx)
--         archedAngle = (y - paddleY) / (paddleLen * 0.7)

maxY :: Float
maxY = 200.0

minY :: Float
minY = -240.0

paddleLen::Float
paddleLen = 80
