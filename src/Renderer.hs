module Renderer(render) where

import Graphics.Gloss
import State

renderBorder :: Picture
renderBorder = pictures [outer, inner] where
    outer = color red $ polygon [(-420,270),(420,270),(420,-270),(-420,-270)]
    inner = color white $ polygon [(-400,250),(400,250),(400,-250),(-400,-250)]

renderPlatform :: Point -> Picture
renderPlatform (x0,y0) = color black $ polygon
    [ (x0       , y0)
    , (x0 + 10.0, y0)
    , (x0 + 10.0, y0 - 60.0)
    , (x0       , y0 - 60.0) ]

renderSeparator :: Picture
renderSeparator = color (greyN 0.7) $ line [(0.0,250.0),(0.0,-250.0)]

renderBall :: Point -> Picture
renderBall (x0,y0) = color black $ thickCircle 6.0 4.0

render :: State -> Picture
render gameState = pictures [ renderBorder
                            , renderSeparator
                            , renderBall $ ballPos gameState
                            , renderPlatform $ fstPlatformPos gameState 
                            , renderPlatform $ sndPlatformPos gameState ]