module Main where

import Graphics.Gloss

import Graphics.Gloss.Data.ViewPort

import Graphics.Gloss.Interface.IO.Game

data PongGame = Game
    { ballLoc :: (Float, Float)
    , ballVel :: (Float, Float)
    , player1 :: Float
    , player2 :: Float
    , paused :: Bool
    , p1Move :: Move
    , p2Move :: Move
    } deriving Show

data Move = UpM | DownM | Stay deriving (Show, Eq)

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
    
main :: IO ()
main = play window background fps initialState render handleKeys update
    where
        fps :: Int
        fps = 60

handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'p') Down _ _) game =
    game {paused = not (paused game)}
handleKeys _ (game @ (Game _ _ _ _ True _ _)) = game

handleKeys (EventKey (Char 's') Down _ _) game = game {p1Move = DownM}
handleKeys (EventKey (Char 's') Up _ _) game
    | p1Move game == DownM = game {p1Move = Stay}
    | otherwise            = game

handleKeys (EventKey (Char 'w') Down _ _) game = game {p1Move = UpM}
handleKeys (EventKey (Char 'w') Up _ _) game
    | p1Move game == UpM   = game {p1Move = Stay}
    | otherwise            = game

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game {p2Move = UpM}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game
    | p2Move game == UpM   = game {p2Move = Stay}
    | otherwise            = game

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game {p2Move = DownM}
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game
    | p2Move game == DownM = game {p2Move = Stay}
    | otherwise            = game

handleKeys _ game = game

update :: Float -> PongGame -> PongGame
update seconds game
    | paused game = game
    | otherwise   = paddleBounce $ wallBounce $ movePaddles seconds $ moveBall seconds game

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
    , ballVel = (30, 30)
    , player1 = 0
    , player2 = 0
    , paused = True
    , p1Move = Stay
    , p2Move = Stay
    }

movePaddles :: Float -> PongGame -> PongGame
movePaddles seconds game = game {player1 = p1', player2 = p2'}
    where
        p1 = player1 game
        p2 = player2 game
        p1' = 50 * p1move + p1
        p2' = 50 * p2move + p2

        p1move
            | (p1Move game == DownM)
              && (p1 >= -fromIntegral height / 2 + 50)
              = -seconds
            | (p1Move game == UpM)
              && (p1 <= fromIntegral height / 2 - 50)
              = seconds
            | otherwise = 0

        p2move
            | (p2Move game == DownM)
              && (p2 >= -fromIntegral height / 2 + 50)
              = -seconds
            | (p2Move game == UpM)
              && (p2 <= fromIntegral height / 2 - 50)
              = seconds
            | otherwise = 0

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