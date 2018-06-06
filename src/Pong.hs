module Pong where

import Graphics.Gloss

import Graphics.Gloss.Data.ViewPort

import Graphics.Gloss.Interface.IO.Game

import Graphics.Gloss.Data.Bitmap

import Data.Maybe

data PongGame = Game
    { ballLoc :: (Float, Float)
    , ballVel :: (Float, Float)
    , player1 :: Float
    , player2 :: Float
    , paused :: Bool
    , buttons :: [Button]
    , p1Move :: Move
    , p2Move :: Move }
              | Menu
    { buttons :: [Button] }
              | Finished
    { player1 :: Float
    , player2 :: Float
    , buttons :: [Button]
    , winner :: String}

data Move = UpM | DownM | Stay deriving (Show, Eq)

data Button = InvButton 
     { position :: RectPos
     , action :: (PongGame -> PongGame) }
            | Button
     { picture :: Picture
     , position :: RectPos
     , action :: (PongGame -> PongGame)
     }

type RectPos = (Position, Position)

type Radius = Float

type Position = (Float, Float)

width, height, offset :: Int
width  = 1000
height = 1000
offset = 10

widthF, heightF :: Float
widthF = fromIntegral width
heightF = fromIntegral height

scaleX, scaleY :: Float
scaleX = widthF / 300
scaleY = heightF / 300

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)
    
background :: Color
background = black

drawButtons :: [Button] -> Picture
drawButtons [] = blank
drawButtons (button : buttons) = pictures
    [ drawButton button
    , drawButtons buttons]

drawButton :: Button -> Picture
drawButton (Button picture _ _) = scale scaleX scaleY picture
drawButton (InvButton _ _) = blank

click :: PongGame -> [Button] -> PongGame
click game [] = game
click game buttons = buttonClick but $ click game buts
    where
        (but : buts) = buttons

buttonsClick :: PongGame -> Position -> PongGame
buttonsClick game pos
    | length buts == 0 = game
    | otherwise        = click game $ clickedButtons buts pos
    where
        buts = buttons game

buttonClick :: Button -> PongGame -> PongGame
buttonClick button game = action button game

clickedButtons :: [Button] -> Position -> [Button]
clickedButtons [] _ = []
clickedButtons (button : buttons) pos
    | clickedButton button pos = button : clickedButtons buttons pos
    | otherwise                = clickedButtons buttons pos

clickedButton :: Button -> Position -> Bool
clickedButton button (x, y) =
    (x >= bx)  && (x <= bx1) &&
    (y >= by1) && (y <= by)
    where
        ((bx, by), (bx1, by1)) = position button

mainScreen :: Picture
mainScreen = pictures [render initialState, buttons]
    where
        buttons :: Picture
        buttons = blank

main :: IO ()
main = do
    play window background fps initialState render handleKeys update
    where
        fps :: Int
        fps = 60

handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'p') Down _ _) (game @ (Game _ _ _ _ _ _ _ _)) =
    game {paused = not (paused game)}
handleKeys _ (game @ (Game _ _ _ _ True _ _ _)) = game

handleKeys (EventKey (Char 's') Down _ _) (game @ (Game _ _ _ _ _ _ _ _)) 
    = game {p1Move = DownM}
handleKeys (EventKey (Char 's') Up _ _) (game @ (Game _ _ _ _ _ _ _ _))
    | p1Move game == DownM = game {p1Move = Stay}
    | otherwise            = game

handleKeys (EventKey (Char 'w') Down _ _) (game @ (Game _ _ _ _ _ _ _ _)) 
    = game {p1Move = UpM}
handleKeys (EventKey (Char 'w') Up _ _) (game @ (Game _ _ _ _ _ _ _ _))
    | p1Move game == UpM   = game {p1Move = Stay}
    | otherwise            = game

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (game @ (Game _ _ _ _ _ _ _ _)) 
    = game {p2Move = UpM}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) (game @ (Game _ _ _ _ _ _ _ _))
    | p2Move game == UpM   = game {p2Move = Stay}
    | otherwise            = game

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) (game @ (Game _ _ _ _ _ _ _ _)) 
    = game {p2Move = DownM}
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) (game @ (Game _ _ _ _ _ _ _ _))
    | p2Move game == DownM = game {p2Move = Stay}
    | otherwise            = game
    
handleKeys (EventKey (MouseButton LeftButton) Down _ pos) game = buttonsClick game pos

handleKeys _ game = game
    
update :: Float -> PongGame -> PongGame
update _ (game @ (Menu _)) = game
update _ (game @ (Finished _ _ _ _)) = game
update seconds game
    | paused game = game
    | otherwise   = checkFinish $ paddleBounce $ wallBounce $ movePaddles seconds $ moveBall seconds game

render :: PongGame -> Picture
render (game @ (Menu _)) = pictures
    [ render initialGameState
    , drawButtons initialButtons]
render game @ (Finished _ _ _ _) = pictures
    [ paddles (player1 game) (player2 game)
    , walls
    , drawButtons finishButtons
    , win $ winner game]
render game = pictures
    [ ball $ ballLoc game
    , walls
    , paddles (player1 game) (player2 game)]

win :: String -> Picture
win winner = 
    scale (0.1 * scaleX) (0.1 * scaleY) $
    color white $
    translate (-500) 300 $ 
    text $ winner ++ " wins"

ball :: (Float, Float) -> Picture
ball (f1, f2) = 
    translate f1 f2 $ 
    scale scaleX scaleY $
    color ballColor $ 
    circleSolid 10
ballColor = dark red

walls = pictures [wall (150 * scaleY), wall (-150 * scaleY)]
wall :: Float -> Picture
wall wallOffset =
    translate 0 wallOffset $
    scale scaleX scaleY $
    color wallColor $
    rectangleSolid 270 10
wallColor = white

paddles :: Float -> Float -> Picture
paddles player1 player2 = pictures 
    [ paddle (-120 * scaleX) player1 rose
    , paddle ( 120 * scaleX) player2 blue]

paddle :: Float -> Float -> Color -> Picture
paddle wallOffset height clr =
    translate wallOffset height $
    scale scaleX scaleY $
    edge clr
    where
        edge :: Color -> Picture
        edge clr = pictures
            [ color (light clr) $ rectangleSolid 20 70
            , color (dark clr) $ rectangleSolid 10 50]

initialGameState :: PongGame  
initialGameState = Game
    { ballLoc = (0, 0)
    , ballVel = (30 * scaleX, 30 * scaleY)
    , player1 = 0
    , player2 = 0
    , paused = False
    , buttons = []
    , p1Move = Stay
    , p2Move = Stay
    }

initialState :: PongGame
initialState = Menu
    { buttons = initialButtons }

initialButtons :: [Button]
initialButtons = [startButton]
    where
        startButton = Button (startButtonPicture) ((-40 * scaleX, 15 * scaleY), (40 * scaleX, -15 * scaleY)) start

startButtonPicture :: Picture
startButtonPicture = pictures
    [ color white (rectangleSolid 80 30)
    , translate (-35) (-10) $
      scale 0.21 0.21 (text "PLAY")]

start :: PongGame -> PongGame
start _ = initialGameState

finishButtons :: [Button]
finishButtons = [restartButton, menuButton]
    where
        restartButton = Button (restartButtonPicture) ((-40 * scaleX, 15 * scaleY), (40 * scaleX, -15 * scaleY)) start
        menuButton = Button (menuButtonPicture) ((-40 * scaleX, -35 * scaleY), (40 * scaleX, -65 * scaleY)) toMenu

restartButtonPicture :: Picture
restartButtonPicture = pictures
    [ color white (rectangleSolid 80 30)
    , translate (-35) (-7) $
      scale 0.13 0.13 (text "RESTART")]

menuButtonPicture :: Picture
menuButtonPicture = pictures
    [ translate 0 (-50) $
      color white (rectangleSolid 80 30)    
    , translate (-36) (-57) $
      scale 0.12 0.12 (text "TO MENU")]

toMenu :: PongGame -> PongGame
toMenu _ = initialState

checkFinish :: PongGame -> PongGame
checkFinish game
    | isJust win = Finished {player1 = p1, player2 = p2, buttons = finishButtons, winner = fromJust win}
    | otherwise  = game
        where
            p1 = player1 game
            p2 = player2 game

            win = finish game

finish :: PongGame -> Maybe String
finish game
    | ballX >  (widthF / 2) - 15 * scaleX = Just "Left player"
    | ballX < -(widthF / 2) + 15 * scaleX = Just "Right player"
    | otherwise                           = Nothing
    where
        (ballX, _) = ballLoc game

movePaddles :: Float -> PongGame -> PongGame
movePaddles seconds game = game {player1 = p1', player2 = p2'}
    where
        p1 = player1 game
        p2 = player2 game
        p1' = 50 * p1move * scaleY + p1
        p2' = 50 * p2move * scaleY + p2

        p1move
            | (p1Move game == DownM)
              && (p1 >= -heightF / 2 + 50 * scaleY)
              = -seconds
            | (p1Move game == UpM)
              && (p1 <= heightF / 2 - 50 * scaleY)
              = seconds
            | otherwise = 0

        p2move
            | (p2Move game == DownM)
              && (p2 >= -heightF / 2 + 50 * scaleY)
              = -seconds
            | (p2Move game == UpM)
              && (p2 <= heightF / 2 - 50 * scaleY)
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
        radius = 10 * scaleX

        (vx, vy) = ballVel game

        vx'
            | paddleCollision game radius = -vx
            | otherwise                   =  vx

paddleCollision :: PongGame -> Radius -> Bool
paddleCollision game radius =
    (leftXCollision radius  && leftCollision) ||
    (rightXCollision radius && rightCollision)
    where
        (x, y) = ballLoc game

        leftCollision = yCollision (player1 game)
        rightCollision = yCollision (player2 game)

        yCollision player = (y <= player + 35 * scaleY)
                         && (y >= player - 35 * scaleY)
        leftXCollision radius  = x - radius <= -widthF / 2 + 40 * scaleX
        rightXCollision radius = x + radius >=  widthF / 2 - 40 * scaleX

wallBounce :: PongGame -> PongGame
wallBounce game = game {ballVel = (vx, vy')}
    where 
        radius = 10 * scaleY

        (vx, vy) = ballVel game

        vy' 
            | wallCollision (ballLoc game) radius = -vy
            | otherwise                           =  vy

wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
    where
        topCollision    = y + radius >=  heightF / 2 - 5 * scaleY
        bottomCollision = y - radius <= -heightF / 2 + 5 * scaleY