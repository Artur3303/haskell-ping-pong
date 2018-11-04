module State(State(..), initialGameState) where

import Graphics.Gloss

data State = State
    { fstPlatformPos   :: Point
    , sndPlatformPos   :: Point
    , ballPos          :: Point
    , score            :: (Int,Int)
    , time             :: Float
    , keyWisPressed    :: Bool
    , keySisPressed    :: Bool
    , keyUpIsPressed   :: Bool
    , keyDownIsPressed :: Bool
    } deriving (Show)

initialGameState :: State
initialGameState = State 
    { fstPlatformPos   = (-370.0, -20.0)
    , sndPlatformPos   = (360.0, -20.0)
    , ballPos          = (0.0, -50.0)
    , score            = (0,0)
    , time             = 0.0
    , keyWisPressed    = False
    , keySisPressed    = False
    , keyUpIsPressed   = False
    , keyDownIsPressed = False
    }