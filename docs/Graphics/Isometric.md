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

#### `pyramid`

``` purescript
pyramid :: Point -> Number -> Number -> Shape
```

A pyramid, determined by a corner point, base-length and height.

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
translateX :: Angle -> Scene -> Scene
```

Translate a scene by a given offset in x-direction.

#### `translateY`

``` purescript
translateY :: Angle -> Scene -> Scene
```

Translate a scene by a given offset in y-direction.

#### `translateZ`

``` purescript
translateZ :: Angle -> Scene -> Scene
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


