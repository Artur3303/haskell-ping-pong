module State(State(..), initialGameState) where

import Graphics.Gloss
import System.Random

data State = State
    { leftPlatformPos  :: Point
    , rightPlatformPos :: Point
    , ballPos          :: Point
    , ballSpeed        :: (Float,Float)
    , score            :: (Int,Int)
    , time             :: Float
    , keyWisPressed    :: Bool
    , keySisPressed    :: Bool
    , keyUpIsPressed   :: Bool
    , keyDownIsPressed :: Bool
    , suspended :: Bool
    , gen :: StdGen
    } deriving (Show)

initialGameState :: State
initialGameState = State 
    { leftPlatformPos  = (-370.0,-20.0)
    , rightPlatformPos = (360.0,-20.0)
    , ballPos          = (0.0,-50.0)
    , ballSpeed        = (4.0, 1.0)
    , score            = (0,0)
    , time             = 0.0
    , keyWisPressed    = False
    , keySisPressed    = False
    , keyUpIsPressed   = False
    , keyDownIsPressed = False
    , suspended        = True
    , gen              = mkStdGen 1
    }

