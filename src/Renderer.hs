module Renderer(render) where

import Graphics.Gloss
import State

renderBorder :: Picture
renderBorder = pictures [outer, inner] where
    outer = color red $ polygon [ (-420,220)
                                , (420,220)
                                , (420,-320)
                                , (-420,-320)
                                ]
    inner = color white $ polygon [ (-400,200)
                                  , (400,200)
                                  , (400,-300)
                                  , (-400,-300)
                                  ]

renderPlatform :: Point -> Picture
renderPlatform (x0,y0) = color black $ polygon
    [ (x0       , y0)
    , (x0 + 10.0, y0)
    , (x0 + 10.0, y0 - 60.0)
    , (x0       , y0 - 60.0)
    ]

renderSeparator :: Picture
renderSeparator = color (greyN 0.7) $ line [(0.0,200.0),(0.0,-300.0)]

renderBall :: Point -> Picture
renderBall (x,y) = translate x y $ color black $ thickCircle 6.0 4.0

renderScore :: (Int,Int) -> Picture
renderScore (u1,u2) = color black $ pictures [colon, left, right] where
    colon = translate (-14.0) 250.0 $ text ":"
    left  = translate (-250.0) 230.0   $ text $ show u1
    right = translate 170.0 230.0   $ text $ show u2

render :: State -> Picture
render gameState = pictures [ renderBorder
                            , renderSeparator
                            , renderBall      $ ballPos          gameState
                            , renderPlatform  $ leftPlatformPos  gameState 
                            , renderPlatform  $ rightPlatformPos gameState
                            , renderScore     $ score            gameState
                            ]