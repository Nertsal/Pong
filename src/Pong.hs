module Pong where

import Graphics.Gloss

import Graphics.Gloss.Data.ViewPort

import Graphics.Gloss.Interface.IO.Game

import Graphics.Gloss.Data.Bitmap

import Graphics.Gloss.Data.Vector

import Data.Maybe

import Data.List

--import Test.HUnit

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
    , playerRadius :: Float}

data Ball = Ball
    { ballPosition :: Point
    , ballRadius :: Float
    , ballVelocity :: Point
    , ballColor :: Color}

data PongGame = Game
    { ball :: Ball
    , player1 :: Player
    , player2 :: Player
    , bonus :: Bonus
    , paused :: Bool
    , buttons :: [Button]
    , p1Move :: Move
    , p2Move :: Move
    , paddlesSpeed :: Float}
              | Menu
    { buttons :: [Button] }
              | Finished
    { player1 :: Player
    , player2 :: Player
    , buttons :: [Button]
    , winner :: String}

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

--test1 = TestCase (assertEqual "for collision" True (bonusCollision initialGameState))

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

mainScreen :: Picture
mainScreen = pictures [render initialState, buttons]
    where
        buttons :: Picture
        buttons = blank

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
render (game @ (Menu _)) = pictures
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
            [ color (light clr) $ translate (-sizex) 0 $ arcSolid 90 270 (playerRadius player) --sectorWire arcDegree (360 - arcDegree) (playerRadius player)
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

        pradius = psizey --psizex + 10 * gameScale

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
checkFinish game
    | isJust win = Finished {player1 = p1, player2 = p2, buttons = finishButtons, winner = fromJust win}
    | otherwise  = game
        where
            p1 = player1 game
            p2 = player2 game

            win = finish game

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
        && (player >= -wallHeight + 45 * gameScale) -- -size / 2 + 50 * gameScale)
        = -seconds
    | (playerMove == UpM)
        && (player <=  wallHeight - 45 * gameScale) -- size / 2 - 50 * gameScale)
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

    {-where
        ball = ballLoc game
        ballVector = ballVel game
        paddle1 = player1 game
        paddle2 = player2 game
        
        str = show normal

        normal = snd $ smallestDepth depths (100000, (0, 0))
        vector' = ballVector - mulSV (2 * dotV ballVector normal) normal

        play1 = (paddle1, -paddlePlace)
        play2 = (paddle2, paddlePlace)

        p1Vectors = 
            snd (smallestVector (getVectors (getPaddleCorners play1) ball) (100000, (0, 0))) :
            [(1, 0), (0, 1), (-1, 0), (0, -1)]
        p2Vectors =
            snd (smallestVector (getVectors (getPaddleCorners play2) ball) (100000, (0, 0))) :
            [(1, 0), (0, 1), (-1, 0), (0, -1)]

        depths = p1Depths' ++ p2Depths'

        p1Depths'
            | notNegative p1D >= 3 = p1Ds
            | otherwise            = []
            where
                p1D = [depths | depths <- map fst p1Depths]
                p1Ds = [depths | depths <- p1Depths, fst depths >= 0]
        
        p2Depths'
            | notNegative p2D >= 3 = p2Ds
            | otherwise            = []
            where
                p2D = [depths | depths <- map fst p2Depths]
                p2Ds = [depths | depths <- p2Depths, fst depths >= 0]

        p1Depths = collisionDepths play1 ball p1Vectors
        p2Depths = collisionDepths play2 ball p2Vectors

notNegative :: (Ord a, Num a) => [a] -> Int
notNegative [] = 0
notNegative (x : xs)
    | (x >= 0)  = 1 + (notNegative xs)
    | otherwise = notNegative xs

collisionDepths :: Player -> Ball -> [Vector] -> [(Float, Vector)]
collisionDepths _ _ [] = []
collisionDepths player ball (vector : vectors) = (depth, vector) : collisionDepths player ball vectors
    where
        depth = getCollisionDepth player ball vector
-}
{-    where
        radius = 10 * gameScale

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

        yCollision player = (y <= player + 35 * gameScale)
                         && (y >= player - 35 * gameScale)
        leftXCollision radius  = x - radius <= -paddlePlace + 10 * gameScale
        rightXCollision radius = x + radius >=  paddlePlace - 10 * gameScale
-}
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

{-
getPaddleCorners :: Player -> [Point]
getPaddleCorners player = [(leftX, upY), (rightX, upY), (rightX, downY), (leftX, downY)]
    where
        (pX, pY) = player

        leftX  = pX - 10 * gameScale
        rightX = pX + 10 * gameScale
        upY    = pY + 35 * gameScale
        downY  = pY - 35 * gameScale

getVectors :: [Point] -> Ball -> [Vector]
getVectors [] _ = []
getVectors positions ball = (vector : getVectors positions' ball)
    where
        (position : positions') = positions
        vector = normalizeV $ getVector (position, ball)

getVector :: (Point, Point) -> Vector
getVector vect = (vx, vy)
    where
        ((x, y), (x', y')) = vect
        vx = x' - x
        vy = y' - y

getLength :: Vector -> Vector -> Float
getLength vector vector' = magV vector * cos (angleVV vector vector')

getCollisionDepth :: Player -> Ball -> Vector -> Float
getCollisionDepth player ball vector = collision' --100000
    where
        collision'
            | notNegatives == 1 = max collisionA collisionB
            | otherwise         = -10
            where
                notNegatives = notNegative [collisionA, collisionB]

        collisionA = ballMax - playerMin
        collisionB = ballMin - playerMax
 --           , collision playerRightMin ballMin vector
 --           , collision playerRightMax ballMin vector
 --           ]

        playerMin = head sortedProjectionCorners
        playerMax = last sortedProjectionCorners

        sortedProjectionCorners = quicksort projectionCorners
        projectionCorners = map (dotV vector) corners
        corners = getPaddleCorners player
        --[playerLeftMax, playerRightMax, playerRightMin, playerLeftMin] = getPaddleCorners player
        ballMax = dotV ball vector + radius
        ballMin = dotV ball vector - radius
        radius = 10 * gameScale

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

smallest :: (Ord a, Num a) => [a] ->  a
smallest [] = 100000
smallest [depth] = depth
smallest (depth : depths) = min depth $ smallest depths --depths depth
    --depths minDepth

smallestVector :: [Vector] -> (Float, Vector) -> (Float, Vector)
smallestVector [] length = length
smallestVector (vector : vectors) (minLength, minVector)
    | length < minLength = smallestVector vectors (length, vector)
    | otherwise          = smallestVector vectors (minLength, minVector)
    where
        length = magV vector

smallestDepth :: [(Float, Vector)] -> (Float, Vector) -> (Float, Vector)
smallestDepth [] depth = depth
smallestDepth ((depth, vector) : depths) (minDepth, minVector)
    | depth < minDepth = smallestDepth depths (depth, vector)
    | otherwise        = smallestDepth depths (minDepth, minVector)
-}