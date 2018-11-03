module State(State(..), initialGameState) where

import Graphics.Gloss

data State = State
    { fstPlatformPos :: Point
    , sndPlatformPos :: Point
    , ballPos        :: Point
    , score			 :: (Int,Int)
	}

initialGameState :: State
initialGameState = State 
    { fstPlatformPos = (-370.0,30.0)
    , sndPlatformPos = (360.0, 30.0)
    , ballPos        = (0.0,0.0)
    , score 		 = (0,0)
	}