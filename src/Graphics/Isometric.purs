module Graphics.Isometric
  ( Face()
  , Shape()
  , extrude
  , extrudeCone
  , prism
  , cube
  , cone
  , pyramid
  , pyramid3
  , cylinder
  , Scene(..)
  , translateX
  , translateY
  , translateZ
  , rotateX
  , rotateY
  , rotateZ
  , scale
  , filled
  , renderScene
  , module Graphics.Isometric.Types
  , module ColorType
  ) where

import Prelude

import Data.Array (sortBy, cons, (..), reverse, zipWith)
import Data.Array.Unsafe (unsafeIndex, last)
import Data.Foldable (foldMap, sum)
import Data.Int (toNumber)
import Data.List (List(..), singleton, (:))
import Data.Monoid (class Monoid, mempty)
import Data.Ord (comparing)

import Math (pi, asin, sqrt, sin, cos)

import Graphics.Drawing as TwoD
import Graphics.Drawing.Color (Color, lighten)
import Graphics.Drawing.Color (Color) as ColorType

import Graphics.Isometric.Point (point, vector, dot, normalize, cross, depth,
                                 from2D)
import Graphics.Isometric.Point as P
import Graphics.Isometric.Types (Angle, Point, Vector)

-- | A `Face` is a list of (at least) three points which all lie in a plane.
type Face = Array Point

-- | A Shape consists of several `Face`s.
type Shape = Array Face

-- | Project a 2D path onto the xy plane and extrude it by the given height.
extrude :: Array TwoD.Point -> Number -> Shape
extrude path2D height = [bottom, reverse top] <> sides
  where path = from2D <$> path2D
        bottom = path
        top = raise <$> bottom
        sides = zipWith side path (last path `cons` path)
        side p1 p2 = [p1, p2, raise p2, raise p1]
        raise = P.translateZ height

-- | Create a cone-like object by extruding the points from a base path in the
-- | xy plane to a single point above it.
extrudeCone :: Array TwoD.Point -> Number -> Shape
extrudeCone path2D height = bottom `cons` sides
  where path = from2D <$> path2D
        bottom = path
        sides = zipWith side path (last path `cons` path)
        side p1 p2 = [p1, p2, tip]
        tip = { x: 0.0, y: 0.0, z: height }

-- | Move a given shape by a vector.
move :: Vector -> Shape -> Shape
move v = map (map (P.translate v))

-- | A prism, constructed from a given corner point and the width, height and
-- | depth (dimensions in x, y, and z-direction).
prism :: Point -> Number -> Number -> Number -> Shape
prism p dx dy dz = move p $ extrude rectangle dz
  where rectangle = [ { x: 0.0, y: 0.0 }
                    , { x: 0.0, y: dy  }
                    , { x: dx,  y: dy  }
                    , { x: dx,  y: 0.0 } ]

-- | A cube is a prism with three equal sides.
cube :: Point -> Number -> Shape
cube p dl = prism p dl dl dl

-- | Draw a (2D) polygon with a given radius and number of vertices.
polygon :: Int -> Number -> Array TwoD.Point
polygon num r = do
  j <- 0 .. (num - 1)
  let phi = - 2.0 * pi / toNumber num * toNumber j
  return { x: r * cos phi, y : r * sin phi }

-- | A cone, determined by the center of the bottom face, the number of sides,
-- | the radius and the height.
cone :: Point -> Int -> Number -> Number -> Shape
cone p num r height = move p $ extrudeCone (polygon num r) height

-- | A four-sided pyramid, determined by the center of the bottom face, the
-- | base length and the height.
pyramid :: Point -> Number -> Number -> Shape
pyramid p = cone p 4

-- | A three-sided pyramid, determined by the center of the bottom face, the
-- | base length and the height.
pyramid3 :: Point -> Number -> Number -> Shape
pyramid3 p len = cone p 3 (len * sqrt 3.0 / 3.0)

-- | A cylinder, determined by the center of the bottom face, the number of
-- | sides, the radius and the height.
cylinder :: Point -> Int -> Number -> Number -> Shape
cylinder p num r height = move p $ extrude (polygon num r) height

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
translateX :: Number -> Scene -> Scene
translateX dx = transform (P.translateX dx)

-- | Translate a scene by a given offset in y-direction.
translateY :: Number -> Scene -> Scene
translateY dy = transform (P.translateY dy)

-- | Translate a scene by a given offset in z-direction.
translateZ :: Number -> Scene -> Scene
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
