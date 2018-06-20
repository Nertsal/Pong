{-# LANGUAGE RecordWildCards #-}
module Pong.Controls where

import           Graphics.Gloss.Interface.Pure.Game
import           Pong.Model

handleMoveKey :: Move -> KeyState -> Player -> Player
handleMoveKey move keyState player = case keyState of
    Down -> setPlayerMove move player
    Up   -> unsetPlayerMove move player

handleEvents :: Event -> PongGame -> PongGame
handleEvents
  (EventKey (Char 'p') Down _ _)
  (GameInProgress game@Game{..})
    = GameInProgress game { gamePaused = not gamePaused }

handleEvents _ game@(GameInProgress Game{ gamePaused = True }) = game

handleEvents (EventKey (Char 's') keyState _ _) (GameInProgress game@Game{..})
  = GameInProgress game {gameLeftPlayer = handleMoveKey DownM keyState gameLeftPlayer }

handleEvents (EventKey (Char 'w') keyState _ _) (GameInProgress game@Game{..})
  = GameInProgress game { gameLeftPlayer = handleMoveKey UpM keyState gameLeftPlayer }

handleEvents (EventKey (SpecialKey KeyUp) keyState _ _) (GameInProgress game@Game{..})
  = GameInProgress game { gameRightPlayer = handleMoveKey UpM keyState gameRightPlayer }

handleEvents (EventKey (SpecialKey KeyDown) keyState _ _) (GameInProgress game@Game{..})
  = GameInProgress game { gameRightPlayer = handleMoveKey DownM keyState gameRightPlayer }

handleEvents (EventKey (MouseButton LeftButton) Down _ mouse) game
  = buttonsClick game mouse

handleEvents _ game = game
