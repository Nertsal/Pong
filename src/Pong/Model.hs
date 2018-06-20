{-# LANGUAGE RecordWildCards #-}
module Pong.Model where

import           Data.List                  (minimumBy)
import           Data.Maybe                 (catMaybes, isJust)
import           Data.Ord                   (comparing)
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector (dotV, mulSV)
import           Pong.Collidable
import           Pong.Const

instance Collidable Player where
    project player v = (minimum projects, maximum projects)
        where
            projects = (centerProject + r) : (centerProject - r) : (map (dotV v) $ corners player)
            centerProject = dotV (playerPosition player) v
            r
                | playerR < 0 = playerR - sizex
                | otherwise   = playerR + sizex
            playerR = playerRadius player
            (sizex, _) = playerSize player

    importantPoints player = (playerPosition player) : corners player

    importantVectors _ = [(1, 0), (-1, 0), (0, 1), (0, -1)]

instance Collidable Ball where
    project Ball{..} v = (centerProject - r, centerProject + r)
        where
            centerProject = dotV ballPosition v
            r = ballRadius

    importantPoints gameBall = [ballPosition gameBall]

    importantVectors _ = []

data Player = Player
    { playerPosition     :: Point
    , playerSize         :: Point
    , playerRadius       :: Float
    , playerMove         :: Move
    , playerMaxSpeed :: Float
    }

data Ball = Ball
    { ballPosition :: Point
    , ballRadius   :: Float
    , ballVelocity :: Point
    , ballColor    :: Color
    }

data PongGame
  = GameInProgress Game
  | GameMenu Menu
  | GameOver GameResult

data Game = Game
  { gameBall        :: Ball
  , gameLeftPlayer  :: Player
  , gameRightPlayer :: Player
  , gameBonus       :: Bonus
  , gamePaused      :: Bool
  , gameButtons     :: [Button]
  }

data Menu = Menu
  { menuButtons :: [Button] }

data GameResult = GameResult
  { gameResultButtons :: [Button]
  , winner            :: String
  }

data Move = UpM | DownM | Stay deriving (Show, Eq)

data Button = Button
     { picture      :: Picture
     , position     :: RectPos
     , buttonAction :: (PongGame -> PongGame)
     }

data Bonus = Bonus
    { base        :: Ball
    , bonusAction :: Game -> Game
    }

type RectPos = (Point, Point)

type Radius = Float

drawButtons :: [Button] -> Picture
drawButtons [] = blank
drawButtons (button : buttons) = pictures
    [ drawButton button
    , drawButtons buttons]

drawButton :: Button -> Picture
drawButton button = scale gameScale gameScale (picture button)

click :: PongGame -> [Button] -> PongGame
click game [] = game
click game buttons = buttonClick but $ click game buts
    where
        (but : buts) = buttons

buttonsClick :: PongGame -> Point -> PongGame
buttonsClick game pos = case getButtons game of
    []   -> game
    buts -> click game $ clickedButtons buts pos

getButtons :: PongGame -> [Button]
getButtons (GameInProgress game) = gameButtons game
getButtons (GameMenu menu)       = menuButtons menu
getButtons (GameOver res)        = gameResultButtons res

buttonClick :: Button -> PongGame -> PongGame
buttonClick button game = buttonAction button game

clickedButtons :: [Button] -> Point -> [Button]
clickedButtons [] _ = []
clickedButtons (button : buttons) pos
    | clickedButton button pos = button : clickedButtons buttons pos
    | otherwise                = clickedButtons buttons pos

clickedButton :: Button -> Point -> Bool
clickedButton button (x, y) =
    (x >= bx)  && (x <= bx1) &&
    (y >= by1) && (y <= by)
    where
        ((bx, by), (bx1, by1)) = position button

update :: Float -> PongGame -> PongGame
update seconds (GameInProgress game)
    | gamePaused game = GameInProgress game
    | otherwise   = checkFinish $ bonusHit $ bounce $ wallBounce $ movePaddles seconds $ moveBall seconds game
update _ game = game

initialGameState :: PongGame
initialGameState = GameInProgress Game
    { gameBall = Ball ballLoc radius ballVel ballClr
    , gameLeftPlayer  = Player (-paddlePlace, 0) (psizex, psizey) pradius Stay 0
    , gameRightPlayer = Player (paddlePlace, 0) (psizex, psizey) (-pradius) Stay 0
    , gameBonus = Bonus (Ball (100, -200) 10 (0, 0) bonusClr) baction
    , gamePaused = False
    , gameButtons = []}
    where
        ballLoc = (0, 0)
        ballVel = (30 * gameScale, 30 * gameScale)
        radius = 10 * gameScale
        ballClr = dark red

        psizex = 5 * gameScale
        psizey = 30 * gameScale

        pradius = psizey

        baction game = game {gameBall = Ball (0, 0) 50 (-50, -50) (dark red)}
        bonusClr = dark green

initialState :: PongGame
initialState = GameMenu Menu
    { menuButtons = initialButtons }

initialButtons :: [Button]
initialButtons = [startButton]
    where
        startButton = Button (startButtonPicture) ((-40 * gameScale, 15 * gameScale), (40 * gameScale, -15 * gameScale)) start

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
        restartButton =
            Button (restartButtonPicture) ((-40 * gameScale, 15 * gameScale), (40 * gameScale, -15 * gameScale)) start
        menuButton =
            Button (menuButtonPicture) ((-40 * gameScale, -35 * gameScale), (40 * gameScale, -65 * gameScale)) toMenu

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

checkFinish :: Game -> PongGame
checkFinish game =
  case finish game of
    Just message -> GameOver GameResult
      { gameResultButtons = finishButtons
      , winner = message
      }
    Nothing -> GameInProgress game

finish :: Game -> Maybe String
finish game
    | ballX >  paddlePlace + 20 * gameScale = Just "Left player"
    | ballX < -paddlePlace - 20 * gameScale = Just "Right player"
    | otherwise                           = Nothing
    where
        (ballX, _) = ballPosition $ gameBall game

movePaddles :: Float -> Game -> Game
movePaddles seconds game = game {gameLeftPlayer = gameLeftPlayer', gameRightPlayer = gameRightPlayer'}
    where
        speed = playerMaxSpeed $ gameLeftPlayer game
        p1 = playerPosition $ gameLeftPlayer game
        p2 = playerPosition $ gameRightPlayer game

        p1y = snd p1
        p2y = snd p2

        p1' = speed * paddleMove seconds p1y (playerMove $ gameLeftPlayer game) * gameScale + p1y
        p2' = speed * paddleMove seconds p2y (playerMove $ gameRightPlayer game) * gameScale + p2y

        gameLeftPlayer' = (gameLeftPlayer game) {playerPosition = (fst p1, p1'), playerMaxSpeed = speed'}
        gameRightPlayer' = (gameRightPlayer game) {playerPosition = (fst p2, p2'), playerMaxSpeed = speed'}

        speed' = (max (abs $ fst ballVel) (abs $ snd ballVel)) + 10
        ballVel = ballVelocity $ gameBall game

paddleMove :: Float -> Float -> Move -> Float
paddleMove seconds player playerMove
    | (playerMove == DownM)
        && (player >= -wallHeight + 45 * gameScale)
        = -seconds
    | (playerMove == UpM)
        && (player <=  wallHeight - 45 * gameScale)
        = seconds
    | otherwise = 0

moveBall :: Float -> Game -> Game
moveBall seconds game = game {gameBall = gameBall'}
    where
        b = gameBall game
        gameBall' = b {ballVelocity = (vx', vy'), ballPosition = (x', y')}
        (x, y) = ballPosition b
        (vx, vy) = ballVelocity b

        x' = x + vx * seconds
        y' = y + vy * seconds

        vx'
            | vx > 0    = vx + seconds * 10
            | vx < 0    = vx - seconds * 10
            | otherwise = vx
        vy'
            | vy > 0    = vy + seconds * 10
            | vx < 0    = vy - seconds * 10
            | otherwise = vy

wallBounce :: Game -> Game
wallBounce game = game {gameBall = gameBall'}
    where
        radius = ballRadius b
        b = gameBall game
        (vx, vy) = ballVelocity b
        (_, by) = ballPosition b
        gameBall' = b {ballVelocity = (vx, vy')}

        vy'
            | (by < 0) && (vy < 0) && collision
                || (by > 0) && (vy > 0) && collision = -vy
            | otherwise                              =  vy

        collision = wallCollision (ballPosition b) radius

wallCollision :: Point -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
    where
        topCollision    = y + radius >=  wallHeight - 5 * gameScale
        bottomCollision = y - radius <= -wallHeight + 5 * gameScale

bonusHit :: Game -> Game
bonusHit game
    | bonusCollision game = bonusAction (gameBonus game) game
    | otherwise           = game

bonusCollision :: Game -> Bool
bonusCollision game = isJust collision
    where
        collision = getCollision (base $ gameBonus game) (gameBall game)

bounce :: Game -> Game
bounce game = game {gameBall = gameBall'}
    where
        gameBall' = b {ballVelocity = reflectedVector, ballPosition = bounceBallPosition}
        reflectedVector = reflectV ballV normalV
        b = gameBall game
        p1 = gameLeftPlayer game
        p2 = gameRightPlayer game
        ballV = ballVelocity b

        bounceBallPosition = ballPosition b + mulSV (snd minVectorDepth) normalV
        normalV = fst minVectorDepth

        minVectorDepth
            | null collisions = (0, 0)
            | otherwise       = minimumBy (comparing snd) collisions

        collisions = catMaybes [getCollision p1 b, getCollision p2 b]

reflectV :: Vector -> Vector -> Vector
reflectV va vb = va - mulSV (2 * dotV va vb) vb

corners :: Player -> [Point]
corners player = [pos + size1, pos - size1, pos + size2, pos - size2]
    where
        pos = playerPosition player
        size1 = playerSize player
        size2 = (fst size1, -(snd size1))

setPlayerMove :: Move -> Player -> Player
setPlayerMove move player = player { playerMove = move }

unsetPlayerMove :: Move -> Player -> Player
unsetPlayerMove move player@Player { playerMove = oldMove } = player { playerMove = newMove }
    where
        newMove
            | oldMove == move = Stay
            | otherwise       = oldMove