module Pong.Collidable where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

class Collidable a where
  -- | Project object on given vector
  project :: a -> Vector -> Segment
  -- | Points which the collision will be checked on
  importantPoints :: a -> [Point]
  -- | Vectors which the collision will be checked on
  importantVectors :: a -> [Vector]

-- | Two extreme points on vector
type Segment = (Float, Float)

-- | Collision depth on vector
type Collision = (Vector, Float)

-- | Collison depth between two objects on given vector
getCollisionDepth :: (Collidable a, Collidable b) => a -> b -> Vector -> Float
getCollisionDepth a b v = snd (project a v) - fst (project b v)

-- | All vectors which the collision will be checked on
getImportantVectors :: (Collidable a, Collidable b) => a -> b -> [Vector]
getImportantVectors a b =
  map normalizeV $
  importantVectors a ++ importantVectors b ++ importantVectorsAB
  where
    importantVectorsAB =
      concat
        [[p2 - p1, p1 - p2] | p1 <- importantPoints a, p2 <- importantPoints b]

-- | Collision between two objects
getCollision :: (Collidable a, Collidable b) => a -> b -> Maybe Collision
getCollision a b
  | snd collision < 0 = Nothing
  | otherwise = Just collision
  where
    collision = minimumBy (comparing snd) collisions
    collisions =
      map (\v -> (v, getCollisionDepth a b v)) $ getImportantVectors a b