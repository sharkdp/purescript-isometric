## Module Graphics.Isometric.Point

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

#### `vector`

``` purescript
vector :: Point -> Point -> Vector
```

Construct a vector as a difference between two points.

#### `dot`

``` purescript
dot :: Vector -> Vector -> Number
```

The dot-product of two vectors.

#### `norm`

``` purescript
norm :: Vector -> Number
```

The Euclidean norm of a vector.

#### `normalize`

``` purescript
normalize :: Vector -> Vector
```

Normalize a vector to length 1.

#### `cross`

``` purescript
cross :: Vector -> Vector -> Vector
```

The cross-product of two vectors.

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

#### `scale`

``` purescript
scale :: Number -> Point -> Point
```

Scale a point by a multiplicative factor (with respect to the origin).

#### `depth`

``` purescript
depth :: Point -> Number
```

Calculate the 'depth' of a point, measured from the point of the observer.


