{-# LANGUAGE RecordWildCards #-}

module Pong.Render where

import           Data.Function  ((&))
import           Graphics.Gloss

import           Pong.Const
import           Pong.Model

-- | Draw current game state
render :: PongGame -> Picture
render (GameMenu Menu{..}) = drawButtons menuButtons
render (GameOver GameResult{..}) =
  pictures [walls, drawButtons gameResultButtons, win winner]
render (GameInProgress Game {..}) =
  pictures
    [ drawBalls gameBalls
    , drawBonuses gameBonuses
    , walls
    , paddles gameLeftPlayer gameRightPlayer
    ]

-- | Draw given buttons
drawButtons :: [Button] -> Picture
drawButtons = scale gameScale gameScale . pictures . map buttonPicture

-- | Draw every bonus in game
drawBonuses :: [Bonus] -> Picture
drawBonuses = pictures . map drawBonus

drawBonus :: Bonus -> Picture
drawBonus Bonus {..} = drawBall bonusBase

-- | Draw winner
win :: String -> Picture
win winner = text (winner ++ " wins")
  & color white
  & translate (-500) 300
  & scale (0.1 * gameScale) (0.1 * gameScale)

-- | Draw every ball in game
drawBalls :: [Ball] -> Picture
drawBalls = pictures . map drawBall

drawBall :: Ball -> Picture
drawBall Ball{..} = circleSolid ballRadius
  & color ballColor
  & translate x y
  where
    (x, y) = ballPosition

-- | Draw walls
walls :: Picture
walls = pictures [wall wallHeight, wall (-wallHeight)]

wall :: Float -> Picture
wall wallOffset = rectangleSolid 400 10 -- FIXME: constants
  & color white                         -- FIXME: constant
  & scale gameScale gameScale
  & translate 0 wallOffset

-- | Draw players
paddles :: Player -> Player -> Picture
paddles player1 player2 =
  pictures [paddle player1 rose 180, paddle player2 blue 0]

paddle :: Player -> Color -> Float -> Picture
paddle player paddleColor theta
  = edge paddleColor
  & rotate theta
  & translate wallOffset playerHeight
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
