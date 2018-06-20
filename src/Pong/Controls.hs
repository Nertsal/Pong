module Pong.Controls where

import           Graphics.Gloss.Interface.Pure.Game
import           Pong.Model

handleEvents :: Event -> PongGame -> PongGame
handleEvents (EventKey (Char 'p') Down _ _) (game @ (Game _ _ _ _ _ _ _ _ _)) =
    game {paused = not (paused game)}
handleEvents _ (game @ (Game _ _ _ _ True _ _ _ _)) = game

handleEvents (EventKey (Char 's') Down _ _) (game @ (Game _ _ _ _ _ _ _ _ _))
    = game {p1Move = DownM}
handleEvents (EventKey (Char 's') Up _ _) (game @ (Game _ _ _ _ _ _ _ _ _))
    | p1Move game == DownM = game {p1Move = Stay}
    | otherwise            = game

handleEvents (EventKey (Char 'w') Down _ _) (game @ (Game _ _ _ _ _ _ _ _ _))
    = game {p1Move = UpM}
handleEvents (EventKey (Char 'w') Up _ _) (game @ (Game _ _ _ _ _ _ _ _ _))
    | p1Move game == UpM   = game {p1Move = Stay}
    | otherwise            = game

handleEvents (EventKey (SpecialKey KeyUp) Down _ _) (game @ (Game _ _ _ _ _ _ _ _ _))
    = game {p2Move = UpM}
handleEvents (EventKey (SpecialKey KeyUp) Up _ _) (game @ (Game _ _ _ _ _ _ _ _ _))
    | p2Move game == UpM   = game {p2Move = Stay}
    | otherwise            = game

handleEvents (EventKey (SpecialKey KeyDown) Down _ _) (game @ (Game _ _ _ _ _ _ _ _ _))
    = game {p2Move = DownM}
handleEvents (EventKey (SpecialKey KeyDown) Up _ _) (game @ (Game _ _ _ _ _ _ _ _ _))
    | p2Move game == DownM = game {p2Move = Stay}
    | otherwise            = game

handleEvents (EventKey (MouseButton LeftButton) Down _ pos) game = buttonsClick game pos

handleEvents _ game = game
