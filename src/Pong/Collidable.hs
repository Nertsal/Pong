module Pong.Collidable where

import           Data.List                  (minimumBy)
import           Data.Ord                   (comparing)
import           Graphics.Gloss.Data.Point
import           Graphics.Gloss.Data.Vector

type Segment = (Float, Float)

type Collision = (Vector, Float)

class Collidable a where
    project :: a -> Vector -> Segment
    importantPoints :: a -> [Point]
    importantVectors :: a -> [Vector]

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
    collision = minimumBy (comparing snd) collisions
    collisions = map (\v -> (v, getCollisionDepth a b v)) $ getImportantVectors a b
