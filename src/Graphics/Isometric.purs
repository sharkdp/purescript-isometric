module Graphics.Isometric
  ( Point(..)
  , Vector(..)
  , Angle(..)
  , isometricAngle
  , point
  , origin
  , translateX
  , translateY
  , translateZ
  , rotateX
  , rotateY
  , rotateZ
  , Face()
  , Shape()
  , prism
  , cube
  , Scene()
  , filled
  , renderScene
  ) where

import Prelude

import Data.Array (sortBy)
import Data.Array.Unsafe (unsafeIndex)
import Data.Foldable (foldMap, sum)
import Data.List (List(..), singleton, (:))
import Data.Monoid (Monoid, mempty)
import Data.Ord (comparing)

import Math (sin, cos, pi, asin, sqrt)

import Graphics.Drawing as TwoD
import Graphics.Drawing.Color

-- | A 3D point.
type Point = { x :: Number
             , y :: Number
             , z :: Number
             }

-- | A vector or direction.
type Vector = Point

-- | An angle in radians.
type Angle = Number

-- | The rotation angle that leads to a symmetric viewpoint.
isometricAngle :: Angle
isometricAngle = pi / 2.0 - asin (1.0 / sqrt 3.0)

-- | Construct a point from x, y, and z coordinates.
point :: Number -> Number -> Number -> Point
point x y z = { x, y, z }

-- | The origin of the 3D coordinate system.
origin :: Point
origin = { x: 0.0, y: 0.0, z: 0.0 }

-- | Construct a vector as a difference between two points.
vector :: Point -> Point -> Vector
vector { x: x1, y: y1, z: z1 } { x: x2, y: y2, z: z2 } =
  { x: x2 - x1
  , y: y2 - y1
  , z: z2 - z1 }

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

-- | The ross-product of two vectors.
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

-- | Calculate the 'depth' of a point, measured from the point of the observer.
depth :: Point -> Number
depth p = p `dot` { x: 1.0, y: 1.0, z: 1.0 }

-- | A `Face` is a list of (at least) three points which all lie in a plane.
type Face = Array Point

-- | A Shape consists of several `Face`s.
type Shape = Array Face

-- | A prism, constructed from a given corner point and the width, height and
-- | depth (dimensions in x, y, and z-direction).
prism :: Point -> Number -> Number -> Number -> Shape
prism p dx dy dz = [ faceZ, translateZ dz <$> faceZ
                   , faceY, translateY dy <$> faceY
                   , faceX, translateX dx <$> faceX]
  where faceZ = [ point p.x p.y p.z, point (p.x + dx) p.y p.z
                , point (p.x + dx) (p.y + dy) p.z, point p.x (p.y + dy) p.z ]
        faceY = [ point p.x p.y p.z, point p.x p.y (p.z + dz)
                , point (p.x + dx) p.y (p.z + dz), point (p.x + dx) p.y p.z ]
        faceX = [ point p.x p.y p.z, point p.x (p.y + dy) p.z
                , point p.x (p.y + dy) (p.z + dz), point p.x p.y (p.z + dz) ]

-- | A cube is a prism with three equal sides.
cube :: Point -> Number -> Shape
cube p dl = prism p dl dl dl

-- | Main data type for the description of a 3D isometric scene.
data Scene = Fill Color Shape
           | Many (List Scene)

instance semigroupScene :: Semigroup Scene where
  append (Many ss) s = Many (ss ++ singleton s)
  append s (Many ss) = Many (s : ss)
  append s1 s2       = Many (Cons s1 (Cons s2 Nil))

instance monoidScene :: Monoid Scene where
  mempty = Many mempty

-- | Fill a shape with a given base color. Individual faces can be brighter.
filled :: Color -> Shape -> Scene
filled = Fill

-- | Isometric projection of a 3D point onto the 2D plane. The angle determines
-- | the rotation around the horizontal axis.
project :: Angle -> Point -> TwoD.Point
project angle p = { x: - rotated.x, y: rotated.y }
  where rotated = rotateX angle $ rotateZ beta $ p
        beta = pi / 4.0

-- | Render a single 3D face.
renderFace :: Angle -> Vector -> Color -> Face -> TwoD.Drawing
renderFace angle dir color face = TwoD.filled (TwoD.fillColor col) $ TwoD.closed (project angle <$> face)
  where col = lighten amount color
        amount = 0.2 + 0.2 * dir `dot` normal
        normal = normalize ((vector p1 p2) `cross` (vector p1 p3))
        p1 = unsafeIndex face 0
        p2 = unsafeIndex face 1
        p3 = unsafeIndex face 2

-- | Render a 3D shape.
renderShape :: Angle -> Vector -> Color -> Shape -> TwoD.Drawing
renderShape angle dir color faces = foldMap (renderFace angle dir color) sortedFaces
  where sortedFaces = sortBy (comparing totalDepth) faces
        totalDepth face = sum (depth <$> face)

-- | Render a three-dimensional `Scene` into a two-dimensional `Drawing`,
-- | using an isometric projection. The angle determines the projection angle
-- | (rotation around the horizontal axis) and the vector determines the
-- | direction of light.
renderScene :: Angle -> Vector -> Scene -> TwoD.Drawing
renderScene angle dir scene = go scene
  where go (Fill color shape) = renderShape angle dir' color shape
        go (Many ss) = foldMap go ss

        dir' = normalize dir
