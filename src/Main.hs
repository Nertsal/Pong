module Main where

import Graphics.Gloss

widht, height, offset :: Int
widht = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (widht, height) (offset, offset)
    
background :: Color
background = black
    
drawing :: Picture
drawing = pictures
    [ color ballColor $ circleSolid 30
    , color paddleColor $ rectangleSolid 10 50
    ]
    where
        ballColor = dark red
        paddleColor = light (light blue)
    
main :: IO ()
main = display window background drawing