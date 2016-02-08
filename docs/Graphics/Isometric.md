## Module Graphics.Isometric

#### `Face`

``` purescript
type Face = Array Point
```

A `Face` is a list of (at least) three points which all lie in a plane.

#### `Shape`

``` purescript
type Shape = Array Face
```

A Shape consists of several `Face`s.

#### `extrude`

``` purescript
extrude :: Array Point -> Number -> Shape
```

Project a 2D path onto the xy plane and extrude it by the given height.

#### `extrudeCone`

``` purescript
extrudeCone :: Array Point -> Number -> Shape
```

Create a cone-like object by extruding the points from a base path in the
xy plane to a single point above it.

#### `prism`

``` purescript
prism :: Point -> Number -> Number -> Number -> Shape
```

A prism, constructed from a given corner point and the width, height and
depth (dimensions in x, y, and z-direction).

#### `cube`

``` purescript
cube :: Point -> Number -> Shape
```

A cube is a prism with three equal sides.

#### `cone`

``` purescript
cone :: Point -> Int -> Number -> Number -> Shape
```

A cone, determined by the center of the bottom face, the number of sides,
the radius and the height.

#### `pyramid`

``` purescript
pyramid :: Point -> Number -> Number -> Shape
```

A four-sided pyramid, determined by the center of the bottom face, the
base length and the height.

#### `pyramid3`

``` purescript
pyramid3 :: Point -> Number -> Number -> Shape
```

A three-sided pyramid, determined by the center of the bottom face, the
base length and the height.

#### `cylinder`

``` purescript
cylinder :: Point -> Int -> Number -> Number -> Shape
```

A cylinder, determined by the center of the bottom face, the number of
sides, the radius and the height.

#### `Scene`

``` purescript
data Scene
  = Fill Color Shape
  | Many (List Scene)
```

Main data type for the description of a 3D isometric scene.

##### Instances
``` purescript
Semigroup Scene
Monoid Scene
```

#### `translateX`

``` purescript
translateX :: Number -> Scene -> Scene
```

Translate a scene by a given offset in x-direction.

#### `translateY`

``` purescript
translateY :: Number -> Scene -> Scene
```

Translate a scene by a given offset in y-direction.

#### `translateZ`

``` purescript
translateZ :: Number -> Scene -> Scene
```

Translate a scene by a given offset in z-direction.

#### `rotateX`

``` purescript
rotateX :: Angle -> Scene -> Scene
```

Rotate a scene around the x-axis.

#### `rotateY`

``` purescript
rotateY :: Angle -> Scene -> Scene
```

Rotate a scene around the y-axis.

#### `rotateZ`

``` purescript
rotateZ :: Angle -> Scene -> Scene
```

Rotate a scene around the z-axis.

#### `scale`

``` purescript
scale :: Number -> Scene -> Scene
```

Scale a scene by a multiplicative factor (with respect to the origin).

#### `filled`

``` purescript
filled :: Color -> Shape -> Scene
```

Fill a shape with a given base color. Individual faces can be brighter.

#### `renderScene`

``` purescript
renderScene :: Vector -> Scene -> Drawing
```

Render a three-dimensional `Scene` into a two-dimensional `Drawing`, using
an isometric projection. The vector determines the direction of light.


### Re-exported from Color:

#### `Color`

``` purescript
data Color
```

The representation of a color.

##### Instances
``` purescript
Show Color
Eq Color
```

### Re-exported from Graphics.Isometric.Types:

#### `Angle`

``` purescript
type Angle = Number
```

An angle in radians.

#### `Point`

``` purescript
type Point = { x :: Number, y :: Number, z :: Number }
```

A 3D point.

#### `Vector`

``` purescript
type Vector = Point
```

A vector or direction.

