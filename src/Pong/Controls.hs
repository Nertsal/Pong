{-# LANGUAGE RecordWildCards #-}
module Pong.Controls where

import           Graphics.Gloss.Interface.Pure.Game
import           Pong.Model

handleEvents :: Event -> PongGame -> PongGame
handleEvents
  (EventKey (Char 'p') Down _ _)
  (GameInProgress game@Game{..})
    = GameInProgress game { paused = not paused }

handleEvents _ game@(GameInProgress Game{ paused = True }) = game

handleEvents (EventKey (Char 's') Down _ _) (GameInProgress game)
  = GameInProgress game { p1Move = DownM }
handleEvents (EventKey (Char 's') Up _ _) (GameInProgress game)
  | p1Move game == DownM = GameInProgress game {p1Move = Stay}
  | otherwise            = GameInProgress game

handleEvents (EventKey (Char 'w') Down _ _) (GameInProgress game)
  = GameInProgress game { p1Move = UpM }
handleEvents (EventKey (Char 'w') Up _ _) (GameInProgress game)
  | p1Move game == UpM = GameInProgress game {p1Move = Stay}
  | otherwise          = GameInProgress game

handleEvents (EventKey (SpecialKey KeyUp) Down _ _) (GameInProgress game)
  = GameInProgress game { p2Move = UpM }
handleEvents (EventKey (SpecialKey KeyUp) Up _ _) (GameInProgress game)
  | p2Move game == UpM = GameInProgress game {p2Move = Stay}
  | otherwise          = GameInProgress game

handleEvents (EventKey (SpecialKey KeyDown) Down _ _) (GameInProgress game)
  = GameInProgress game { p2Move = DownM }
handleEvents (EventKey (SpecialKey KeyDown) Up _ _) (GameInProgress game)
  | p2Move game == DownM = GameInProgress game {p2Move = Stay}
  | otherwise            = GameInProgress game

handleEvents (EventKey (MouseButton LeftButton) Down _ mouse) game
  = buttonsClick game mouse

handleEvents _ game = game
