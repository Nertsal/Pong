module Main where

import Graphics.Gloss

import Graphics.Gloss.Data.ViewPort

data PongGame = Game
    { ballLoc :: (Float, Float)
    , ballVel :: (Float, Float)
    , player1 :: Float
    , player2 :: Float
    } deriving Show

type Radius = Float

type Position = (Float, Float)

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)
    
background :: Color
background = black
    
drawing :: Picture
drawing = pictures
    [ ball
    , walls
    , paddles]
    where
        ball = color ballColor $ circleSolid 10
        ballColor = dark red

        walls = pictures [wall 150, wall (-150)]
        wall :: Float -> Picture
        wall offset = 
            translate 0 offset $
            color wallColor $
            rectangleSolid 270 10
        wallColor = white

        paddles = pictures [paddle (-120) rose, paddle 120 blue]
        paddle :: Float -> Color -> Picture
        paddle offset clr =
            translate offset 0 $
            edge clr
            where
                edge :: Color -> Picture
                edge clr = pictures
                    [ color (light clr) $ rectangleSolid 20 70
                    , color (dark clr) $ rectangleSolid 10 50]        
    
main :: IO ()
main = simulate window background fps initialState render update
    where
        fps :: Int
        fps = 60

update :: ViewPort -> Float -> PongGame -> PongGame
update _ seconds = paddleBounce . wallBounce . moveBall seconds

render :: PongGame -> Picture
render game = pictures
    [ ball $ ballLoc game
    , walls
    , paddles (player1 game) (player2 game)]
    where
        ball :: (Float, Float) -> Picture
        ball (f1, f2) = translate f1 f2 $ color ballColor $ circleSolid 10
        ballColor = dark red

        walls = pictures [wall 150, wall (-150)]
        wall :: Float -> Picture
        wall offset = 
            translate 0 offset $
            color wallColor $
            rectangleSolid 270 10
        wallColor = white

        paddles :: Float -> Float -> Picture
        paddles player1 player2 = pictures 
            [ paddle (-120) player1 rose
            , paddle 120 player2 blue]

        paddle :: Float -> Float -> Color -> Picture
        paddle offset height clr =
            translate offset height $
            edge clr
            where
                edge :: Color -> Picture
                edge clr = pictures
                    [ color (light clr) $ rectangleSolid 20 70
                    , color (dark clr) $ rectangleSolid 10 50]

initialState :: PongGame  
initialState = Game
    { ballLoc = (0, 0)
    , ballVel = (0, 30)
    , player1 = 0
    , player2 = 0
    }

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game {ballLoc = (x', y')}
    where
        (x, y) = ballLoc game
        (vx, vy) = ballVel game

        x' = x + vx * seconds
        y' = y + vy * seconds

paddleBounce :: PongGame -> PongGame
paddleBounce game = game {ballVel = (vx', vy)}
    where
        radius = 10

        (vx, vy) = ballVel game

        vx' = if paddleCollision game radius
              then -vx
              else  vx

paddleCollision :: PongGame -> Radius -> Bool
paddleCollision game radius = xCollision radius && (leftCollision || rightCollision)
    where
        (x, y) = ballLoc game

        leftCollision = yCollision (player1 game)
        rightCollision = yCollision (player2 game)

        yCollision player = (y <= player + 35) && (y >= player - 35)
        xCollision radius = (x - radius <= -fromIntegral width / 2 + 40)
                         || (x + radius >=  fromIntegral width / 2 - 40)

wallBounce :: PongGame -> PongGame
wallBounce game = game {ballVel = (vx, vy')}
    where 
        radius = 10

        (vx, vy) = ballVel game

        vy' = if wallCollision (ballLoc game) radius
              then -vy
              else  vy

wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
    where
        topCollision    = y + radius >=  fromIntegral height / 2 - 4
        bottomCollision = y - radius <= -fromIntegral height / 2 + 4