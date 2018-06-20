{-# LANGUAGE RecordWildCards #-}
module Pong (main) where

import           Data.List
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Interface.IO.Game

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
    project Ball{..} v = (centerProject - r, centerProject + r)
        where
            centerProject = dotV ballPosition v
            r = ballRadius

    importantPoints ball = [ballPosition ball]

    importantVectors _ = []

data Player = Player
    { playerPosition :: Point
    , playerSize     :: Point
    , playerRadius   :: Float}

data Ball = Ball
    { ballPosition :: Point
    , ballRadius   :: Float
    , ballVelocity :: Point
    , ballColor    :: Color}

data PongGame = Game
    { ball         :: Ball
    , player1      :: Player
    , player2      :: Player
    , bonus        :: Bonus
    , paused       :: Bool
    , buttons      :: [Button]
    , p1Move       :: Move
    , p2Move       :: Move
    , paddlesSpeed :: Float}
              | Menu
    { buttons :: [Button] }
              | Finished
    { player1 :: Player
    , player2 :: Player
    , buttons :: [Button]
    , winner  :: String}

data Move = UpM | DownM | Stay deriving (Show, Eq)

data Button = Button
     { picture      :: Picture
     , position     :: RectPos
     , buttonAction :: (PongGame -> PongGame)
     }

data Bonus = Bonus
    { base        :: Ball
    , bonusAction :: (PongGame -> PongGame)}

type Collision = (Vector, Float)

type Segment = (Float, Float)

type RectPos = (Point, Point)

type Radius = Float

width, height, offset :: Int
width  = 1500
height = 1000
offset = 10

size, widthF, heightF :: Float
widthF  = fromIntegral width
heightF = fromIntegral height
size
    | widthF / 1.5  < heightF = widthF
    | heightF <= widthF / 1.5 = heightF

gameScale :: Float
gameScale
    | widthF / 1.5  < heightF  = size / 450
    | heightF <= widthF / 1.5 = size / 300

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
click game buttons = buttonClick but $ click game buts
    where
        (but : buts) = buttons

buttonsClick :: PongGame -> Point -> PongGame
buttonsClick game pos
    | length buts == 0 = game
    | otherwise        = click game $ clickedButtons buts pos
    where
        buts = buttons game

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

main :: IO ()
main = do
    play window background fps initialState render handleEvents update
    where
        fps :: Int
        fps = 60

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

update :: Float -> PongGame -> PongGame
update _ (game @ (Menu _)) = game
update _ (game @ (Finished _ _ _ _)) = game
update seconds game
    | paused game = game
    | otherwise   = checkFinish $ bonusHit $ bounce $ wallBounce $ movePaddles seconds $ moveBall seconds game

render :: PongGame -> Picture
render (Menu _) = pictures
    [ render initialGameState
    , drawButtons initialButtons]
render game @ (Finished _ _ _ _) = pictures
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

walls :: Picture
walls = pictures [wall wallHeight, wall (-wallHeight)]

wall :: Float -> Picture
wall wallOffset =
  translate 0 wallOffset $
  scale gameScale gameScale $
  color wallColor $
  rectangleSolid 400 10
    where
      wallColor = white

paddles :: Player -> Player -> Picture
paddles player1 player2 = pictures
    [ paddle player1 rose 180
    , paddle player2 blue 0]

paddle :: Player -> Color -> Float -> Picture
paddle player paddleColor rot = translate wallOffset playerHeight $ rotate rot $ edge paddleColor
    where
        (wallOffset, playerHeight) = playerPosition player

        (sizex, sizey) = playerSize player

        edge :: Color -> Picture
        edge clr = pictures
            [ color (light clr) $ translate (-sizex) 0 $ arcSolid 90 270 (playerRadius player)
            , color (light clr) $ rectangleSolid (sizex * 2) (sizey * 2)
            , color (dark clr) $ rectangleSolid sizex sizey]

initialGameState :: PongGame
initialGameState = Game
    { ball = Ball ballLoc radius ballVel ballClr
    , player1 = Player (-paddlePlace, 0) (psizex, psizey) pradius
    , player2 = Player (paddlePlace, 0) (psizex, psizey) (-pradius)
    , bonus = Bonus (Ball (100, -200) 10 (0, 0) bonusClr) baction
    , paused = False
    , buttons = []
    , p1Move = Stay
    , p2Move = Stay
    , paddlesSpeed = 50}
    where
        ballLoc = (0, 0)
        ballVel = (30 * gameScale, 30 * gameScale)
        radius = 10 * gameScale
        ballClr = dark red

        psizex = 5 * gameScale
        psizey = 30 * gameScale

        pradius = psizey

        baction game = game {ball = Ball (0, 0) 50 (-50, -50) (dark red)}
        bonusClr = dark green

initialState :: PongGame
initialState = Menu
    { buttons = initialButtons }

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

checkFinish :: PongGame -> PongGame
checkFinish game =
  case finish game of
    Just message -> Finished
      { player1 = p1
      , player2 = p2
      , buttons = finishButtons
      , winner = message
      }
    Nothing -> game
  where
    p1 = player1 game
    p2 = player2 game

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

        p1' = speed * paddleMove seconds p1y (p1Move game) * gameScale + p1y
        p2' = speed * paddleMove seconds p2y (p2Move game) * gameScale + p2y

        player1' = (player1 game) {playerPosition = (fst p1, p1')}
        player2' = (player2 game) {playerPosition = (fst p2, p2')}

        speed' = speed + seconds * 5

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
corners player = [pos + size1, pos - size1, pos + size2, pos - size2]
    where
        pos = playerPosition player
        size1 = playerSize player
        size2 = (fst size1, -(snd size1))

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
