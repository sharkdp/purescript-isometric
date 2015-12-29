## Module Graphics.Isometric

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

#### `Angle`

``` purescript
type Angle = Number
```

An angle in radians.

#### `isometricAngle`

``` purescript
isometricAngle :: Angle
```

The rotation angle that leads to a symmetric viewpoint.

#### `point`

``` purescript
point :: Number -> Number -> Number -> Point
```

Construct a point from x, y, and z coordinates.

#### `origin`

``` purescript
origin :: Point
```

The origin of the 3D coordinate system.

#### `translateX`

``` purescript
translateX :: Number -> Point -> Point
```

Translate a point by a given offset in x-direction.

#### `translateY`

``` purescript
translateY :: Number -> Point -> Point
```

Translate a point by a given offset in y-direction.

#### `translateZ`

``` purescript
translateZ :: Number -> Point -> Point
```

Translate a point by a given offset in z-direction.

#### `rotateX`

``` purescript
rotateX :: Angle -> Point -> Point
```

Rotate a point around the x-axis.

#### `rotateY`

``` purescript
rotateY :: Angle -> Point -> Point
```

Rotate a point around the y-axis.

#### `rotateZ`

``` purescript
rotateZ :: Angle -> Point -> Point
```

Rotate a point around the z-axis.

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

#### `Scene`

``` purescript
data Scene
```

Main data type for the description of a 3D isometric scene.

##### Instances
``` purescript
Semigroup Scene
Monoid Scene
```

#### `filled`

``` purescript
filled :: Color -> Shape -> Scene
```

Fill a shape with a given base color. Individual faces can be brighter.

#### `renderScene`

``` purescript
renderScene :: Angle -> Vector -> Scene -> Drawing
```

Render a three-dimensional `Scene` into a two-dimensional `Drawing`,
using an isometric projection. The angle determines the projection angle
(rotation around the horizontal axis) and the vector determines the
direction of light.


