module Graphics.Isometric.Point
  ( point
  , origin
  , from2D
  , vector
  , translate
  , dot
  , norm
  , normalize
  , cross
  , translateX
  , translateY
  , translateZ
  , rotateX
  , rotateY
  , rotateZ
  , scale
  , depth
  ) where

import Prelude

import Math (sin, cos, sqrt)

import Graphics.Isometric.Types (Point, Angle, Vector)
import Graphics.Drawing as TwoD

-- | Construct a point from x, y, and z coordinates.
point :: Number -> Number -> Number -> Point
point x y z = { x, y, z }

-- | The origin of the 3D coordinate system.
origin :: Point
origin = { x: 0.0, y: 0.0, z: 0.0 }

-- | Construct a 3D point in the xy plane from a 2D point.
from2D :: TwoD.Point -> Point
from2D { x, y } = { x, y, z: 0.0 }

-- | Construct a vector as a difference between two points.
vector :: Point -> Point -> Vector
vector { x: x1, y: y1, z: z1 } { x: x2, y: y2, z: z2 } =
  { x: x2 - x1
  , y: y2 - y1
  , z: z2 - z1 }

-- | Translate a given point by a vector.
translate :: Point -> Vector -> Point
translate { x: x1, y: y1, z: z1 } { x: x2, y: y2, z: z2 } =
  { x: x1 + x2
  , y: y1 + y2
  , z: z1 + z2 }

-- | The dot-product of two vectors.
dot :: Vector -> Vector -> Number
dot { x: x1, y: y1, z: z1 } { x: x2, y: y2, z: z2 } =
  x1 * x2 + y1 * y2 + z1 * z2

-- | The Euclidean norm of a vector.
norm :: Vector -> Number
norm p = sqrt (p `dot` p)

-- | Normalize a vector to length 1.
normalize :: Vector -> Vector
normalize p = { x: p.x / n, y: p.y / n, z: p.z / n }
  where n = norm p

-- | The cross-product of two vectors.
cross :: Vector -> Vector -> Vector
cross { x: x1, y: y1, z: z1 } { x: x2, y: y2, z: z2 } =
  { x: y1 * z2 - z1 * y2
  , y: z1 * x2 - x1 * z2
  , z: x1 * y2 - y1 * x2 }

-- | Translate a point by a given offset in x-direction.
translateX :: Number -> Point -> Point
translateX dx { x, y, z } = { x: x + dx, y: y, z: z }

-- | Translate a point by a given offset in y-direction.
translateY :: Number -> Point -> Point
translateY dy { x, y, z } = { x: x, y: y + dy, z: z }

-- | Translate a point by a given offset in z-direction.
translateZ :: Number -> Point -> Point
translateZ dz { x, y, z } = { x: x, y: y, z: z + dz }

-- | Rotate a point around the x-axis.
rotateX :: Angle -> Point -> Point
rotateX phi { x, y, z } = { x: x
                          , y: cos phi * y - sin phi * z
                          , z: sin phi * y + cos phi * z }

-- | Rotate a point around the y-axis.
rotateY :: Angle -> Point -> Point
rotateY phi { x, y, z } = { x: cos phi * x + sin phi * z
                          , y: y
                          , z: - sin phi * x + cos phi * z }

-- | Rotate a point around the z-axis.
rotateZ :: Angle -> Point -> Point
rotateZ phi { x, y, z } = { x: cos phi * x - sin phi * y
                          , y: sin phi * x + cos phi * y
                          , z: z }

-- | Scale a point by a multiplicative factor (with respect to the origin).
scale :: Number -> Point -> Point
scale f { x, y, z } = { x: f * x, y: f * y, z: f * z }

-- | Calculate the 'depth' of a point, measured from the point of the observer.
depth :: Point -> Number
depth p = p.x + p.y + p.z
