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
    = GameInProgress game { paused = not paused }

handleEvents _ game@(GameInProgress Game{ paused = True }) = game

handleEvents (EventKey (Char 's') keyState _ _) (GameInProgress game@Game{..})
  = GameInProgress game {player1 = handleMoveKey DownM keyState player1 }

handleEvents (EventKey (Char 'w') keyState _ _) (GameInProgress game@Game{..})
  = GameInProgress game { player1 = handleMoveKey UpM keyState player1 }

handleEvents (EventKey (SpecialKey KeyUp) keyState _ _) (GameInProgress game@Game{..})
  = GameInProgress game { player2 = handleMoveKey UpM keyState player2 }

handleEvents (EventKey (SpecialKey KeyDown) keyState _ _) (GameInProgress game@Game{..})
  = GameInProgress game { player2 = handleMoveKey DownM keyState player2 }

handleEvents (EventKey (MouseButton LeftButton) Down _ mouse) game
  = buttonsClick game mouse

handleEvents _ game = game
