module Graphics.Isometric.Types
  ( Point(..)
  , Vector(..)
  , Angle(..)
  ) where

-- | A 3D point.
type Point = { x :: Number
             , y :: Number
             , z :: Number
             }

-- | A vector or direction.
type Vector = Point

-- | An angle in radians.
type Angle = Number
