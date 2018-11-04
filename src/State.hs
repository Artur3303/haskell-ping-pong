module State(State(..), initialGameState, updateGameState) where

import Graphics.Gloss

data State = State
    { fstPlatformPos :: Point
    , sndPlatformPos :: Point
    , ballPos        :: Point
    , score          :: (Int,Int)
    , time           :: Float
    } deriving (Show)

initialGameState :: State
initialGameState = State 
    { fstPlatformPos = (-370.0, -20.0)
    , sndPlatformPos = (360.0, -20.0)
    , ballPos        = (0.0, -50.0)
    , score          = (0,0)
    , time           = 0.0
    }

updateGameState :: Float -> State -> State
updateGameState secs gameState = gameState { time = secs }