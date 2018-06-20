{-# LANGUAGE RecordWildCards #-}
module Pong.Render where

import           Graphics.Gloss

import           Pong.Const
import           Pong.Model

render :: PongGame -> Picture
render (Menu _) = pictures
    [ render initialGameState
    , drawButtons initialButtons]
render game @ (Finished _ _ _ _) = pictures
    [ paddles (player1 game) (player2 game)
    , walls
    , drawButtons finishButtons
    , win $ winner game]
render game = pictures
    [ ballPicture $ ball game
    , drawBonus $ bonus game
    , walls
    , paddles (player1 game) (player2 game)]

drawBonus :: Bonus -> Picture
drawBonus Bonus{..} = ballPicture base

win :: String -> Picture
win winner =
    scale (0.1 * gameScale) (0.1 * gameScale) $
    color white $
    translate (-500) 300 $
    text $ winner ++ " wins"

ballPicture :: Ball -> Picture
ballPicture ball =
    translate f1 f2 $
    color bColor $
    circleSolid radius
    where
        bColor = ballColor ball
        (f1, f2) = ballPosition ball
        radius = ballRadius ball

walls :: Picture
walls = pictures [wall wallHeight, wall (-wallHeight)]

wall :: Float -> Picture
wall wallOffset =
  translate 0 wallOffset $
  scale gameScale gameScale $
  color wallColor $
  rectangleSolid 400 10
    where
      wallColor = white

paddles :: Player -> Player -> Picture
paddles player1 player2 = pictures
    [ paddle player1 rose 180
    , paddle player2 blue 0]

paddle :: Player -> Color -> Float -> Picture
paddle player paddleColor rot = translate wallOffset playerHeight $ rotate rot $ edge paddleColor
    where
        (wallOffset, playerHeight) = playerPosition player

        (sizex, sizey) = playerSize player

        edge :: Color -> Picture
        edge clr = pictures
            [ color (light clr) $ translate (-sizex) 0 $ arcSolid 90 270 (playerRadius player)
            , color (light clr) $ rectangleSolid (sizex * 2) (sizey * 2)
            , color (dark clr) $ rectangleSolid sizex sizey]


