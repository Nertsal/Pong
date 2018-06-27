{-# LANGUAGE RecordWildCards #-}

module Pong.Render where

import Graphics.Gloss

import Pong.Const
import Pong.Model

-- | Draw current game state
render :: PongGame -> Picture
render (GameMenu _) =
  pictures [render initialGameState, drawButtons initialButtons]
render (GameOver GameResult {..}) =
  pictures [walls, drawButtons finishButtons, win winner]
render (GameInProgress Game {..}) =
  pictures
    [ drawBalls gameBalls
    , drawBonuses gameBonuses
    , walls
    , paddles gameLeftPlayer gameRightPlayer
    ]

-- | Draw every bonus in game
drawBonuses :: [Bonus] -> Picture
drawBonuses bonuses = pictures (map drawBonus bonuses)

drawBonus :: Bonus -> Picture
drawBonus Bonus {..} = drawBall bonusBase

-- | Draw winner
win :: String -> Picture
win winner =
  scale (0.1 * gameScale) (0.1 * gameScale) $
  color white $ translate (-500) 300 $ text $ winner ++ " wins"

-- | Draw every ball in game
drawBalls :: [Ball] -> Picture
drawBalls balls = pictures (map drawBall balls)

drawBall :: Ball -> Picture
drawBall ball = translate f1 f2 $ color bColor $ circleSolid radius
  where
    bColor = ballColor ball
    (f1, f2) = ballPosition ball
    radius = ballRadius ball

-- | Draw walls
walls :: Picture
walls = pictures [wall wallHeight, wall (-wallHeight)]

wall :: Float -> Picture
wall wallOffset =
  translate 0 wallOffset $
  scale gameScale gameScale $ color wallColor $ rectangleSolid 400 10
  where
    wallColor = white

-- | Draw players
paddles :: Player -> Player -> Picture
paddles player1 player2 =
  pictures [paddle player1 rose 180, paddle player2 blue 0]

paddle :: Player -> Color -> Float -> Picture
paddle player paddleColor rot =
  translate wallOffset playerHeight $ rotate rot $ edge paddleColor
  where
    (wallOffset, playerHeight) = playerPosition player
    (sizex, sizey) = playerSize player
    edge :: Color -> Picture
    edge clr =
      pictures
        [ color (light clr) $
          translate (-sizex) 0 $ arcSolid 90 270 (playerRadius player)
        , color (light clr) $ rectangleSolid (sizex * 2) (sizey * 2)
        , color (dark clr) $ rectangleSolid sizex sizey
        ]
