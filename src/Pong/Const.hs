{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Game parameters and constants.
module Pong.Const where

import           Graphics.Gloss

width, height, offset :: Num a => a
width  = 1500
height = 1000
offset = 10

size :: Float
size
  | width / 1.5 < height  = width
  | otherwise             = height

gameScale :: Float
gameScale
  | width / 1.5  < height = size / 450
  | otherwise             = size / 300

wallHeight :: Float
wallHeight = 150 * gameScale

paddlePlace :: Float
paddlePlace = 200 * gameScale

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

