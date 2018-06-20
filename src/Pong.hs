module Pong where

{-# LANGUAGE RecordWildCards #-}

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game

import Graphics.Gloss.Data.Vector

import Data.Maybe

import Data.List

import System.Random

import Control.Monad.Trans.State

--import qualified Test.HUnit as Hunit --for testing

class Collidable a where 
    project :: a -> Vector -> Segment
    importantPoints :: a -> [Point]
    importantVectors :: a -> [Vector]

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
    project ball v = (centerProject - r, centerProject + r)
        where
            centerProject = dotV (ballPosition ball) v
            r = ballRadius ball

    importantPoints ball = [ballPosition ball]

    importantVectors _ = []

data Player = Player
    { playerPosition :: Point
    , playerSize :: Point
    , playerRadius :: Float
    , move :: Move
    , speed :: Float}

data Ball = Ball
    { ballPosition :: Point
    , ballRadius :: Float
    , ballVelocity :: Point
    , ballColor :: Color}

data PongGame
    = GameInProgress Game
    | Menu MenuButtons
    | GameOver GameResults
{-     { ball :: Ball
    , player1 :: Player
    , player2 :: Player
    , bonus :: Bonus
    , paused :: Bool
    , buttons :: [Button]
    , p1Move :: Move
    , p2Move :: Move
    , paddlesSpeed :: Float
    , rndGen :: StdGen }
              | Menu
    { buttons :: [Button]
    , rndGen :: StdGen }
              | Finished
    { player1 :: Player
    , player2 :: Player
    , buttons :: [Button]
    , winner :: String
    , rndGen :: StdGen }
 -}

data Game = Game
    { balls :: [Ball]
    , players :: [Player]
    , bonuses :: [Bonus]
    , gameButtons :: [Button]
    , paused :: Bool
    , gameRandomGeneretor :: StdGen }

data MenuButtons = MenuButtons
    { menuButtons :: [Button]
    , menuRandomGeneretor :: StdGen }

data GameResults = GameResults
    { resultButtons :: [Button]
    , winner :: String
    , resultRandomGeneretor :: StdGen }

data Move = UpM | DownM | Stay deriving (Show, Eq)

data Button = Button
     { picture :: Picture
     , position :: RectPos
     , buttonAction :: (PongGame -> PongGame)
     }

data Bonus = Bonus
    { base :: Ball
    , bonusAction :: (PongGame -> PongGame)}

type Collision = (Vector, Float)

type Segment = (Float, Float)

type RectPos = (Point, Point)

type Radius = Float

--test1 = Hunit.TestCase (Hunit.assertEqual "for collision" (1) (evalState getRandom $ mkStdGen 1)) --for testing

width, height, offset, size :: Num a => a
width  = 1500
height = 1000
offset = 10

size
    | width / 1.5  < height = width
    | height <= width / 1.5 = height

gameScale :: Float
gameScale = case size of
    width -> size / 450
    height -> size / 300

wallHeight :: Float
wallHeight = 150 * gameScale

paddlePlace :: Float
paddlePlace = 200 * gameScale

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
drawButton button = scale gameScale gameScale (picture button)

click :: PongGame -> [Button] -> PongGame
click game [] = game
click game buttons = buttonClick but (click game buts)
    where
        (but : buts) = buttons

buttonsClick :: PongGame -> Point -> PongGame
buttonsClick game pos = case buttons game of
    [] -> game
    buts -> click game (clickedButtons buts pos)

buttonClick :: Button -> PongGame -> PongGame
buttonClick button game = buttonAction button game

clickedButtons :: [Button] -> Point -> [Button]
clickedButtons [] _ = []
clickedButtons (button : buttons) pos
    | clickedButton button pos = button : clicked
    | otherwise                = clicked
    where
        clicked = clickedButtons buttons pos

clickedButton :: Button -> Point -> Bool
clickedButton button (x, y) =
    (x >= bx)  && (x <= bx1) &&
    (y >= by1) && (y <= by)
    where
        ((bx, by), (bx1, by1)) = position button

mainScreen :: Picture
mainScreen = pictures [render menuState, buttons]
    where
        buttons :: Picture
        buttons = blank

main :: IO ()
main = do
    gen <- getStdGen
    let initialState = menuState {menuRandomGeneretor = gen}
    play window background fps initialState render handleEvents update

fps :: Int
fps = 60

handleEvents :: Event -> PongGame -> PongGame
handleEvents (EventKey (Char 'p') Down _ _) game@Game =
    game {paused = not (paused game)}
handleEvents _ game@Game{paused = True} = game

handleEvents (EventKey (Char 's') Down _ _) game@Game
    = game {p1Move = DownM}
handleEvents (EventKey (Char 's') Up _ _) game@Game
    | p1Move game == DownM = game {p1Move = Stay}
    | otherwise            = game

handleEvents (EventKey (Char 'w') Down _ _) game@Game 
    = game {p1Move = UpM}
handleEvents (EventKey (Char 'w') Up _ _) game@Game
    | p1Move game == UpM   = game {p1Move = Stay}
    | otherwise            = game

handleEvents (EventKey (SpecialKey KeyUp) Down _ _) game@Game 
    = game {p2Move = UpM}
handleEvents (EventKey (SpecialKey KeyUp) Up _ _) game@Game
    | p2Move game == UpM   = game {p2Move = Stay}
    | otherwise            = game

handleEvents (EventKey (SpecialKey KeyDown) Down _ _) game@Game
    = game {p2Move = DownM}
handleEvents (EventKey (SpecialKey KeyDown) Up _ _) game@Game
    | p2Move game == DownM = game {p2Move = Stay}
    | otherwise            = game
    
handleEvents (EventKey (MouseButton LeftButton) Down _ pos) game = buttonsClick game pos

handleEvents _ game = game
    
update :: Float -> PongGame -> PongGame
update _ game@MenuButtons = game
update _ game@GameResults = game
update seconds game
    | paused game = game
    | otherwise   = checkFinish $ bonusHit $ bounce $ wallBounce $ movePaddles seconds $ moveBall seconds game

render :: PongGame -> Picture
render game@MenuButtons = pictures
    [ render gameState
    , drawButtons initialButtons]
render game@GameResults = pictures
    [ paddles (player1 game) (player2 game)
    , walls
    , drawButtons finishButtons
    , win $ winner game]
render game = pictures
    [ ballPicture $ ball game
    , drawBonus $ bonus game
    , walls
    , paddles (player1 game) (player2 game)]

drawBonus :: Bonus -> Picture
drawBonus bonus = ballPicture $ base bonus

win :: String -> Picture
win winner = 
    scale (0.1 * gameScale) (0.1 * gameScale) $
    color white $
    translate (-500) 300 $ 
    text $ winner ++ " wins"

ballPicture :: Ball -> Picture
ballPicture ball = 
    translate f1 f2 $
    color bColor $ 
    circleSolid radius
    where
        bColor = ballColor ball
        (f1, f2) = ballPosition ball
        radius = ballRadius ball

walls = pictures [wall wallHeight, wall (-wallHeight)]
wall :: Float -> Picture
wall wallOffset =
    translate 0 wallOffset $
    scale gameScale gameScale $
    color wallColor $
    rectangleSolid 400 10
wallColor = white

paddles :: Player -> Player -> Picture
paddles player1 player2 = pictures 
    [ paddle player1 rose 180
    , paddle player2 blue 0]

paddle :: Player -> Color -> Float -> Picture
paddle player clr rot = translate wallOffset height $ rotate rot $ edge clr
    where
        (wallOffset, height) = playerPosition player

        (sizex, sizey) = playerSize player

        edge :: Color -> Picture
        edge clr = pictures
            [ color (light clr) $ translate (-sizex) 0 $ arcSolid 90 270 (playerRadius player)
            , color (light clr) $ rectangleSolid (sizex * 2) (sizey * 2)
            , color (dark clr) $ rectangleSolid sizex sizey]

gameState :: PongGame  
gameState = Game
    { ball = Ball (0, 0) 0 (0, 0) red
    , player1 = Player (-paddlePlace, 0) (psizex, psizey) pradius
    , player2 = Player (paddlePlace, 0) (psizex, psizey) (-pradius)
    , bonus = Bonus (Ball (0, 0) 0 (0, 0) red) (\g -> g)
    , paused = False
    , buttons = []
    , p1Move = Stay
    , p2Move = Stay
    , paddlesSpeed = fst ballVel + 20
    , rndGen = snd randomBallVelocity}
    where
        ballLoc = (0, 0)
        ballVel = fst randomBallVelocity
        radius = 10 * gameScale
        ballClr = dark red

        psizex = 5 * gameScale
        psizey = 30 * gameScale

        pradius = psizey

        baction game = game {ball = Ball (0, 0) 50 (-50, -50) (dark red)}
        bonusClr = dark green

        randomGen = execState getRandom $ mkStdGen 123
        randomBallVelocity = getBallVelocity randomGen

getGameState :: StdGen -> PongGame
getGameState gen = gameState {ball = randomBall, rndGen = newGen}
    where
        randomBall = Ball ballLoc ballRadius ballVel ballClr

        ballLoc = (0, 0)
        ballRadius = 10 * gameScale
        ballVel = fst rndBallVel
        ballClr = dark red

        rndBallVel = getBallVelocity gen
        newGen = snd rndBallVel

menuState :: PongGame
menuState = Menu
    { buttons = initialButtons
    , rndGen = mkStdGen 123 }

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
start game = getGameState $ rndGen game

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
toMenu game = menuState {rndGen = gen}
    where
        gen = rndGen game

checkFinish :: PongGame -> PongGame
checkFinish game = case finish game of
    Just win -> GameResults {players = [p1, p2], buttons = finishButtons, winner = win, resultRandomGeneretor = gen}
    Nothing -> game
        where
            p1 = player1 game
            p2 = player2 game
            gen = gameRandomGeneretor game

finish :: PongGame -> Maybe String
finish game
    | ballX >  paddlePlace + 20 * gameScale = Just "Left player"
    | ballX < -paddlePlace - 20 * gameScale = Just "Right player"
    | otherwise                           = Nothing
    where
        (ballX, _) = ballPosition $ ball game

movePaddles :: Float -> PongGame -> PongGame
movePaddles seconds game = game {player1 = player1', player2 = player2', paddlesSpeed = speed'}
    where
        speed = paddlesSpeed game
        p1 = playerPosition $ player1 game
        p2 = playerPosition $ player2 game

        p1y = snd p1
        p2y = snd p2

        p1' = speed * paddleMove seconds p1y (p1Move game) + p1y
        p2' = speed * paddleMove seconds p2y (p2Move game) + p2y

        player1' = (player1 game) {playerPosition = (fst p1, p1')}
        player2' = (player2 game) {playerPosition = (fst p2, p2')}

        speed' = (max (abs $ fst ballVel) (abs $ snd ballVel)) + 10

        ballVel = ballVelocity $ ball game

paddleMove :: Float -> Float -> Move -> Float
paddleMove seconds player playerMove
    | (playerMove == DownM)
        && (player >= -wallHeight + 45 * gameScale)
        = -seconds
    | (playerMove == UpM)
        && (player <=  wallHeight - 45 * gameScale)
        = seconds
    | otherwise = 0

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game {ball = ball'}
    where
        b = ball game
        ball' = b {ballVelocity = (vx', vy'), ballPosition = (x', y')}
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

wallBounce :: PongGame -> PongGame
wallBounce game = game {ball = ball'}
    where 
        radius = ballRadius b
        b = ball game
        (vx, vy) = ballVelocity b
        (_, by) = ballPosition b
        ball' = b {ballVelocity = (vx, vy')}

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

bonusHit :: PongGame -> PongGame
bonusHit game
    | bonusCollision game = bonusAction (bonus game) game
    | otherwise           = game

bonusCollision :: PongGame -> Bool
bonusCollision game = isJust collision 
    where
        collision = getCollision (base $ bonus game) (ball game)

bounce :: PongGame -> PongGame
bounce game = game {ball = ball'}
    where
        ball' = b {ballVelocity = reflectedVector, ballPosition = bounceBallPosition}
        reflectedVector = reflectV ballV normalV
        b = ball game
        p1 = player1 game
        p2 = player2 game
        ballV = ballVelocity b

        bounceBallPosition = ballPosition b + mulSV (snd minVectorDepth) normalV
        normalV = fst minVectorDepth

        minVectorDepth
            | null collisions = (0, 0)
            | otherwise       = minimumBy secondCompare collisions

        collisions = catMaybes [getCollision p1 b, getCollision p2 b]

reflectV :: Vector -> Vector -> Vector
reflectV va vb = va - mulSV (2 * dotV va vb) vb

corners :: Player -> [Point]
corners player = [pos + size, pos - size, pos + size2, pos - size2]
    where
        pos = playerPosition player
        size = playerSize player
        size2 = (fst size, -(snd size))

getCollisionDepth :: (Collidable a, Collidable b) => a -> b -> Vector -> Float
getCollisionDepth a b v = snd (project a v) - fst (project b v)

getImportantVectors :: (Collidable a, Collidable b) => a -> b -> [Vector]
getImportantVectors a b = map normalizeV $ importantVectors a ++ importantVectors b ++ importantVectorsAB
    where
        importantVectorsAB = 
            concat [[p2 - p1, p1 - p2] | 
            p1 <- importantPoints a, p2 <- importantPoints b]

getCollision :: (Collidable a, Collidable b) => a -> b -> Maybe Collision
getCollision a b
    | snd collision < 0 = Nothing
    | otherwise         = Just collision
    where
        collision = minimumBy secondCompare collisions
        collisions = map (\v -> (v, getCollisionDepth a b v)) $ getImportantVectors a b

secondCompare :: (Num a, Ord a) => (b, a) -> (b, a) -> Ordering
secondCompare a b = compare (snd a) (snd b)

ballSpeedBonus :: Bonus
ballSpeedBonus = Bonus shape action
    where
        shape = Ball (0, 0) radius (0, 0) bonusClr
        radius = 5
        bonusClr = light blue

        action game = game {ball = speedBall (ball game)}
        speedBall ball = ball {ballVelocity = speedVel (ballVelocity ball) 1.5}
        speedVel vel speed = (fst vel * speed, snd vel * speed)

getRandom :: State StdGen Float
getRandom = do 
    generator <- get
    let (value, newGenerator) = randomR (-100, 100) generator
    put newGenerator
    return value

randomIntR :: (Int, Int) -> StdGen -> (Int, StdGen)
randomIntR range gen = randomR range gen

getBallVelocity :: StdGen -> (Point, StdGen)
getBallVelocity gen = ((rndx, rndy), newGen)
    where
        (rndx, gen2) = randomR (20, 40) gen
        (rndy, newGen) = randomR (-40, 40) gen2

bonuses :: [Bonus]
bonuses = [ballSpeedBonus]

getBonus :: StdGen -> (Bonus, StdGen)
getBonus gen = (bonus, newGen)
    where
        bonus = randomBonus
        randomBonus = bonuses !! (int - 1)
        (int, newGen) = randomIntR (1, length bonuses) gen

getBonusPosition :: StdGen -> (Point, StdGen)
getBonusPosition gen = ((bonusx, bonusy), newGen)
    where
        (bonusx, gen2) = randomR (-300, 300) gen
        (bonusy, newGen) = randomR (-200, 200) gen2