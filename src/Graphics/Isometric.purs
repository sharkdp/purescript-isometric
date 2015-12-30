module Graphics.Isometric
  ( Face()
  , Shape()
  , prism
  , cube
  , pyramid
  , Scene()
  , translateX
  , translateY
  , translateZ
  , rotateX
  , rotateY
  , rotateZ
  , scale
  , filled
  , renderScene
  , module Graphics.Drawing.Color
  , module Graphics.Isometric.Types
  ) where

import Prelude

import Data.Array (sortBy, cons, (..))
import Data.Array.Unsafe (unsafeIndex)
import Data.Foldable (foldMap, sum)
import Data.Int (toNumber)
import Data.List (List(..), singleton, (:))
import Data.Monoid (Monoid, mempty)
import Data.Ord (comparing)

import Math (pi, asin, sqrt)

import Graphics.Drawing as TwoD
import Graphics.Drawing.Color

import Graphics.Isometric.Point (point, vector, dot, normalize, cross, depth)
import Graphics.Isometric.Point as P
import Graphics.Isometric.Types

-- | A `Face` is a list of (at least) three points which all lie in a plane.
type Face = Array Point

-- | A Shape consists of several `Face`s.
type Shape = Array Face

-- | A prism, constructed from a given corner point and the width, height and
-- | depth (dimensions in x, y, and z-direction).
prism :: Point -> Number -> Number -> Number -> Shape
prism p dx dy dz = [ faceZ, P.translateZ dz <$> faceZ
                   , faceY, P.translateY dy <$> faceY
                   , faceX, P.translateX dx <$> faceX]
  where faceZ = [ point p.x p.y p.z, point (p.x + dx) p.y p.z
                , point (p.x + dx) (p.y + dy) p.z, point p.x (p.y + dy) p.z ]
        faceY = [ point p.x p.y p.z, point p.x p.y (p.z + dz)
                , point (p.x + dx) p.y (p.z + dz), point (p.x + dx) p.y p.z ]
        faceX = [ point p.x p.y p.z, point p.x (p.y + dy) p.z
                , point p.x (p.y + dy) (p.z + dz), point p.x p.y (p.z + dz) ]

-- | A cube is a prism with three equal sides.
cube :: Point -> Number -> Shape
cube p dl = prism p dl dl dl

-- | A pyramid, determined by a corner point, base-length and height.
pyramid :: Point -> Number -> Number -> Shape
pyramid p w h = base `cons` map side (0..3)
  where base     = map corner (0..3)
        corner n = rot n (point w2 w2 0.0)
        side n   = rot n <$> [point (-w2) w2 0.0, point w2 w2 0.0, point 0.0 0.0 h]
        rot n    = P.rotateZ (pi / 2.0 * toNumber n)
        w2       = w / 2.0

-- | Main data type for the description of a 3D isometric scene.
data Scene = Fill Color Shape
           | Many (List Scene)

instance semigroupScene :: Semigroup Scene where
  append (Many ss) s = Many (ss ++ singleton s)
  append s (Many ss) = Many (s : ss)
  append s1 s2       = Many (Cons s1 (Cons s2 Nil))

instance monoidScene :: Monoid Scene where
  mempty = Many mempty

-- | Apply a pointwise transformation to a whole scene.
transform :: (Point -> Point) -> Scene -> Scene
transform t = go
  where
    go (Fill color shape) = Fill color (map t <$> shape)
    go (Many ss) = Many (transform t <$> ss)

-- | Translate a scene by a given offset in x-direction.
translateX :: Angle -> Scene -> Scene
translateX dx = transform (P.translateX dx)

-- | Translate a scene by a given offset in y-direction.
translateY :: Angle -> Scene -> Scene
translateY dy = transform (P.translateY dy)

-- | Translate a scene by a given offset in z-direction.
translateZ :: Angle -> Scene -> Scene
translateZ dz = transform (P.translateZ dz)

-- | Rotate a scene around the x-axis.
rotateX :: Angle -> Scene -> Scene
rotateX angle = transform (P.rotateX angle)

-- | Rotate a scene around the y-axis.
rotateY :: Angle -> Scene -> Scene
rotateY angle = transform (P.rotateY angle)

-- | Rotate a scene around the z-axis.
rotateZ :: Angle -> Scene -> Scene
rotateZ angle = transform (P.rotateZ angle)

-- | Scale a scene by a multiplicative factor (with respect to the origin).
scale :: Number -> Scene -> Scene
scale factor = transform (P.scale factor)

-- | Fill a shape with a given base color. Individual faces can be brighter.
filled :: Color -> Shape -> Scene
filled = Fill

-- | The rotation angle that leads to a symmetric viewpoint.
isometricAngle :: Angle
isometricAngle = pi / 2.0 - asin (1.0 / sqrt 3.0)

-- | Isometric projection of a 3D point onto the 2D plane.
project :: Point -> TwoD.Point
project p = { x: - rotated.x, y: rotated.y }
  where rotated = P.rotateX isometricAngle $ P.rotateZ beta $ p
        beta = pi / 4.0

-- | Render a single 3D face.
renderFace :: Vector -> Color -> Face -> TwoD.Drawing
renderFace dir color face = TwoD.filled (TwoD.fillColor col) $
                              TwoD.closed (project <$> face)
  where col = lighten amount color
        amount = 0.2 + 0.2 * dir `dot` normal
        normal = normalize ((vector p1 p2) `cross` (vector p1 p3))
        p1 = unsafeIndex face 0
        p2 = unsafeIndex face 1
        p3 = unsafeIndex face 2

-- | Render a 3D shape with a given fill color.
fillShape :: Vector -> Color -> Shape -> TwoD.Drawing
fillShape dir color faces = foldMap (renderFace dir color) sortedFaces
  where sortedFaces = sortBy (comparing totalDepth) faces
        totalDepth face = sum (depth <$> face)

-- | Render a three-dimensional `Scene` into a two-dimensional `Drawing`, using
-- | an isometric projection. The vector determines the direction of light.
renderScene :: Vector -> Scene -> TwoD.Drawing
renderScene dir scene = go scene
  where go (Fill color shape) = fillShape dir' color shape
        go (Many ss) = foldMap go ss

        dir' = normalize dir
