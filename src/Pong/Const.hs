{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Game parameters and constants.
module Pong.Const where

import Graphics.Gloss

windowWidth, windowHeight, windowOffset :: Num a => a
windowWidth = 1500
windowHeight = 1000
windowOffset = 10

-- | Window size which we will focus on
size :: Float
size
  | windowWidth / 1.5 < windowHeight = windowWidth
  | otherwise = windowHeight

-- | Window scale
gameScale :: Float
gameScale
  | windowWidth / 1.5 < windowHeight = size / 450
  | otherwise = size / 300

-- | Height of the up and depth of down walls
wallHeight :: Float
wallHeight = 150 * gameScale

-- | Distance from center of the screen to the center of players
paddlePlace :: Float
paddlePlace = 200 * gameScale

window :: Display
window = InWindow "Pong" (windowWidth, windowHeight) (windowOffset, windowOffset)

background :: Color
background = black
