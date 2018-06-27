{-# LANGUAGE RecordWildCards #-}

module Pong.Model where

import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Graphics.Gloss
import Graphics.Gloss.Data.Vector (dotV, mulSV)
import Pong.Collidable
import Pong.Const

instance Collidable Player where
  project player v = (minimum projects, maximum projects)
    where
      projects =
        (centerProject + r) :
        (centerProject - r) : (map (dotV v) $ corners player)
      centerProject = dotV (playerPosition player) v
      r
        | playerR < 0 = playerR - sizex
        | otherwise = playerR + sizex
      playerR = playerRadius player
      (sizex, _) = playerSize player
  importantPoints player = (playerPosition player) : corners player
  importantVectors _ = [(1, 0), (-1, 0), (0, 1), (0, -1)]

instance Collidable Ball where
  project Ball {..} v = (centerProject - r, centerProject + r)
    where
      centerProject = dotV ballPosition v
      r = ballRadius
  importantPoints gameBall = [ballPosition gameBall]
  importantVectors _ = []

data Player = Player
  { playerPosition :: Point
  , playerSize :: Point
  , playerRadius :: Float
  , playerMove :: Move
  , playerMaxSpeed :: Float
  }

data Ball = Ball
  { ballPosition :: Point
  , ballRadius :: Float
  , ballVelocity :: Point
  , ballColor :: Color
  }

data PongGame
  = GameInProgress Game
  | GameMenu Menu
  | GameOver GameResult

data Game = Game
  { gameBalls :: [Ball]
  , gameLeftPlayer :: Player
  , gameRightPlayer :: Player
  , gameBonuses :: [Bonus]
  , gamePaused :: Bool
  , gameButtons :: [Button]
  }

data Menu = Menu
  { menuButtons :: [Button]
  }

data GameResult = GameResult
  { gameResultButtons :: [Button]
  , winner :: String
  }

data Move
  = UpM -- ^ Move up
  | DownM -- ^ Move down
  | Stay -- ^ Don't move
  deriving (Show, Eq)

data Button = Button
  { buttonPicture :: Picture
  , buttonPosition :: RectPos
  , buttonAction :: (PongGame -> PongGame)
  }

data Bonus = Bonus
  { bonusBase :: Ball -- ^ Bonus hitbox
  , bonusAction :: Game -> Game -- ^ Bonus hit action
  }

type RectPos = (Point, Point)

type Radius = Float

-- | Draw given buttons
drawButtons :: [Button] -> Picture
drawButtons [] = blank
drawButtons (button:buttons) = pictures [drawButton, drawButtons buttons]
  where
    drawButton = scale gameScale gameScale (buttonPicture button)

-- | Act clicked buttons' actions
click :: PongGame -> [Button] -> PongGame
click game [] = game
click game buttons = buttonClick but $ click game buts
  where
    (but:buts) = buttons

-- | Mouse left click reaction
buttonsClick :: PongGame -> Point -> PongGame
buttonsClick game pos =
  case getButtons game of
    [] -> game
    buts -> click game $ clickedButtons buts pos

-- | All buttons in game
getButtons :: PongGame -> [Button]
getButtons (GameInProgress game) = gameButtons game
getButtons (GameMenu menu) = menuButtons menu
getButtons (GameOver res) = gameResultButtons res

-- | Act button's action
buttonClick :: Button -> PongGame -> PongGame
buttonClick button game = buttonAction button game

-- | Choose clicked buttons
clickedButtons :: [Button] -> Point -> [Button]
clickedButtons [] _ = []
clickedButtons (button:buttons) pos
  | clickedButton button pos = button : clickedButtons buttons pos
  | otherwise = clickedButtons buttons pos

-- | Check if button is clicked
clickedButton :: Button -> Point -> Bool
clickedButton button (x, y) = (x >= bx) && (x <= bx1) && (y >= by1) && (y <= by)
  where
    ((bx, by), (bx1, by1)) = buttonPosition button

-- | Update game by seconds
update :: Float -> PongGame -> PongGame
update seconds (GameInProgress game)
  | gamePaused game = GameInProgress game
  | otherwise =
    checkFinish $
    bonusesHits $
    bounce $ wallBounce $ movePaddles seconds $ moveGameBalls seconds game
update _ game = game

-- | First state of playing
initialGameState :: PongGame
initialGameState =
  GameInProgress
    Game
      { gameBalls = [startBall]
      , gameLeftPlayer =
          Player (-paddlePlace, 0) (psizex, psizey) pradius Stay 0
      , gameRightPlayer =
          Player (paddlePlace, 0) (psizex, psizey) (-pradius) Stay 0
      , gameBonuses = []
      , gamePaused = False
      , gameButtons = []
      }
  where
    startBall = Ball ballLoc radius ballVel startBallColor
    ballLoc = (0, 0)
    ballVel = (30 * gameScale, 30 * gameScale)
    radius = 10 * gameScale
    startBallColor = dark red
    psizex = 5 * gameScale
    psizey = 30 * gameScale
    pradius = psizey

initialButtons :: [Button]
initialButtons = [startButton]
  where
    startButton =
      Button
        (startButtonPicture)
        ((-40 * gameScale, 15 * gameScale), (40 * gameScale, -15 * gameScale))
        start
    
    startButtonPicture =
      pictures
        [ color white (rectangleSolid 80 30)
        , translate (-35) (-10) $ scale 0.21 0.21 (text "PLAY")
        ]

-- | First menu state
initialState :: PongGame
initialState = GameMenu Menu {menuButtons = initialButtons}

-- | Start game
start :: PongGame -> PongGame
start _ = initialGameState

-- | Go to menu
toMenu :: PongGame -> PongGame
toMenu _ = initialState

finishButtons :: [Button]
finishButtons = [restartButton, menuButton]
  where
    restartButton =
      Button
        (restartButtonPicture)
        ((-40 * gameScale, 15 * gameScale), (40 * gameScale, -15 * gameScale))
        start

    restartButtonPicture :: Picture
    restartButtonPicture =
      pictures
        [ color white (rectangleSolid 80 30)
        , translate (-35) (-7) $ scale 0.13 0.13 (text "RESTART")
        ]

    menuButton =
      Button
        (menuButtonPicture)
        ((-40 * gameScale, -35 * gameScale), (40 * gameScale, -65 * gameScale))
        toMenu

    menuButtonPicture =
      pictures
        [ translate 0 (-50) $ color white (rectangleSolid 80 30)
        , translate (-36) (-57) $ scale 0.12 0.12 (text "TO MENU")
        ]

-- | Check if game is finished
checkFinish :: Game -> PongGame
checkFinish game =
  case finish (gameBalls game) of
    Just message ->
      GameOver GameResult {gameResultButtons = finishButtons, winner = message}
    Nothing -> GameInProgress game
  where
    finish :: [Ball] -> Maybe String
    finish [] = Nothing
    finish (ball:balls) =
      case ballFinish ball of
        Just message -> Just message
        Nothing -> finish balls

    ballFinish :: Ball -> Maybe String
    ballFinish ball
      | ballX > paddlePlace + 20 * gameScale = Just "Left player"
      | ballX < -paddlePlace - 20 * gameScale = Just "Right player"
      | otherwise = Nothing
      where
        (ballX, _) = ballPosition ball

-- | Move players by seconds
movePaddles :: Float -> Game -> Game
movePaddles seconds game@Game {..} =
  game {gameLeftPlayer = gameLeftPlayer', gameRightPlayer = gameRightPlayer'}
  where
    speed = playerMaxSpeed gameLeftPlayer
    p1 = playerPosition gameLeftPlayer
    p2 = playerPosition gameRightPlayer
    p1y = snd p1
    p2y = snd p2
    p1' =
      speed * paddleMove seconds p1y (playerMove gameLeftPlayer) * gameScale +
      p1y
    p2' =
      speed * paddleMove seconds p2y (playerMove gameRightPlayer) * gameScale +
      p2y
    gameLeftPlayer' =
      gameLeftPlayer {playerPosition = (fst p1, p1'), playerMaxSpeed = speed'}
    gameRightPlayer' =
      gameRightPlayer {playerPosition = (fst p2, p2'), playerMaxSpeed = speed'}
    speed' = maxBallsSpeed gameBalls - 20 * gameScale

maxBallsSpeed :: [Ball] -> Float
maxBallsSpeed [] = 0
maxBallsSpeed balls = maximum $ map maxBallSpeed balls

maxBallSpeed :: Ball -> Float
maxBallSpeed ball = max (abs $ speedx) (abs $ speedy)
  where
    (speedx, speedy) = ballVelocity ball

paddleMove :: Float -> Float -> Move -> Float
paddleMove seconds player playerMove
  | (playerMove == DownM) && (player >= -wallHeight + 45 * gameScale) = -seconds
  | (playerMove == UpM) && (player <= wallHeight - 45 * gameScale) = seconds
  | otherwise = 0

-- | Move balls by seconds
moveGameBalls :: Float -> Game -> Game
moveGameBalls seconds game =
  game {gameBalls = map (moveBall seconds) (gameBalls game)}

moveBall :: Float -> Ball -> Ball
moveBall seconds Ball {..} =
  Ball
    (movePosition ballPosition ballVelocity seconds)
    ballRadius
    (accelerateSpeed ballVelocity $ (seconds * 10, seconds * 10))
    ballColor

accelerateSpeed :: Point -> Point -> Point
accelerateSpeed (speedx, speedy) (accx, accy) =
  (accelerate speedx accx, accelerate speedy accy)

accelerate :: Float -> Float -> Float
accelerate speed acc
  | speed > 0 = speed + acc
  | speed < 0 = speed - acc
  | otherwise = speed

movePosition :: Point -> Point -> Float -> Point
movePosition (x, y) (speedx, speedy) seconds =
  (x + speedx * seconds, y + speedy * seconds)

-- | Bounce balls from walls
wallBounce :: Game -> Game
wallBounce game = game {gameBalls = map wallBounceBall (gameBalls game)}

wallBounceBall :: Ball -> Ball
wallBounceBall ball@Ball {..} = ball {ballVelocity = (vx, vy')}
  where
    radius = ballRadius
    (vx, vy) = ballVelocity
    (_, by) = ballPosition
    vy'
      | (by < 0) && (vy < 0) && collision || (by > 0) && (vy > 0) && collision =
        -vy
      | otherwise = vy
    collision = wallCollision ballPosition radius

wallCollision :: Point -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision = y + radius >= wallHeight - 5 * gameScale
    bottomCollision = y - radius <= -wallHeight + 5 * gameScale

everyWith :: a -> [b] -> [(a, b)]
everyWith _ [] = []
everyWith a (b:bs) = (a, b) : everyWith a bs

everyWithEvery :: [a] -> [b] -> [(a, b)]
everyWithEvery [] _ = []
everyWithEvery _ [] = []
everyWithEvery (a:as) bs = everyWith a bs ++ everyWithEvery as bs

bonusesHits :: Game -> Game
bonusesHits game@Game {..} =
  foldl bonusHit game (everyWithEvery gameBonuses gameBalls)

bonusHit :: Game -> (Bonus, Ball) -> Game
bonusHit game (bonus, ball)
  | bonusCollision bonus ball = bonusAction bonus game
  | otherwise = game

bonusCollision :: Bonus -> Ball -> Bool
bonusCollision bonus ball =
  case getCollision (bonusBase bonus) ball of
    Just _ -> True
    Nothing -> False

-- | Bounce every object in the game
bounce :: Game -> Game
bounce game = game {gameBalls = map (\b -> bounceBall b game) (gameBalls game)}

bounceBall :: Ball -> Game -> Ball
bounceBall ball@Ball {..} game = bouncedBall
  where
    bouncedBall =
      ball {ballVelocity = reflectedVector, ballPosition = bounceBallPosition}
    reflectedVector = reflectV ballV normalV
    p1 = gameLeftPlayer game
    p2 = gameRightPlayer game
    ballV = ballVelocity
    bounceBallPosition = ballPosition + mulSV (snd minVectorDepth) normalV
    normalV = fst minVectorDepth
    minVectorDepth
      | null collisions = (0, 0)
      | otherwise = minimumBy (comparing snd) collisions
    collisions = catMaybes [getCollision p1 ball, getCollision p2 ball]

reflectV :: Vector -> Vector -> Vector
reflectV va vb = va - mulSV (2 * dotV va vb) vb

corners :: Player -> [Point]
corners player = [pos + size1, pos - size1, pos + size2, pos - size2]
  where
    pos = playerPosition player
    size1 = playerSize player
    size2 = (fst size1, -(snd size1))

-- | Move key is pressed
setPlayerMove :: Move -> Player -> Player
setPlayerMove move player = player {playerMove = move}

-- | Move key is unpressed
unsetPlayerMove :: Move -> Player -> Player
unsetPlayerMove move player@Player {playerMove = oldMove} =
  player {playerMove = newMove}
  where
    newMove
      | oldMove == move = Stay
      | otherwise = oldMove
