{-# LANGUAGE RecordWildCards #-}

module Pong
  ( main
  ) where

import Graphics.Gloss

import Pong.Const
import Pong.Controls
import Pong.Model
import Pong.Render

main :: IO ()
main = play window background fps initialState render handleEvents update
  where
    fps = 60
