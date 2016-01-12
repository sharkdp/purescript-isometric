module Test.Main where

import Prelude

import Data.Array
import Data.Int
import Data.Foldable

import Graphics.Isometric
import Graphics.Isometric.Point as P
import Graphics.Isometric.DepthSort

import Math (sin, cos, pi)

import Signal.DOM

import Flare
import Flare.Drawing as D

-- Example 1

scene1 :: Int -> Number -> Angle -> D.Drawing
scene1 n offset angle =
  D.translate 300.0 250.0 $
    renderScene { x: -4.0, y: -1.0, z: 3.0 } $
      rotateZ angle $ fold do
        i <- 0..n
        j <- 0..n
        let x = toNumber i / toNumber n
            y = toNumber j / toNumber n
            p = { x: dl * toNumber i, y: dl * toNumber j, z: 0.0 }
            h = 2.0 * dl + 1.5 * dl * sin (pi * x + offset) * cos (pi * y + offset)
        return $ filled (D.hsl (300.0 * x) 0.5 0.5) (prism p w w h)
  where dl = 230.0 / toNumber n
        w = 0.9 * dl

-- Example 2

red'   = hsl 0.0 0.6 0.5
green' = hsl 110.0 0.6 0.5

scene2 :: Number -> Number -> D.Drawing
scene2 rotZ time =
  D.translate 300.0 300.0 $
    renderScene { x: -4.0, y: -1.0, z: 3.0 } $
      scale 45.0 $ depthSort $ rotateZ rotZ $
           filled green' (prism (P.point 0.0 pos1 0.0) 2.0 1.0 2.0)
        <> filled red'   (prism (P.point pos2 0.0 0.0) 1.0 2.0 2.0)
        <> filled gray   (prism (P.point (-2.5) (-2.5) (-0.5)) 7.0 7.0 0.5)
  where
    pos1 = 3.0 * cos (0.001 * time) + 0.5
    pos2 = 3.0 * sin (0.001 * time) + 0.5

-- Example 3

p :: Int -> Int -> Int -> Point
p x y z = { x: toNumber x, y: toNumber y, z: toNumber z }

scene3 :: Number -> D.Drawing
scene3 angle =
  D.translate 300.0 300.0 $
    renderScene { x: -4.0, y: -1.0, z: 3.0 } $
      scale 45.0 $ rotateZ angle $
        foldMap (\pos -> filled (hsl 210.0 0.8 0.5) (cube pos 1.0))
          [ p 1 1 0 , p 1 2 0 , p 1 3 0 , p 2 3 0
          , p 3 3 0 , p 0 (-2) 0 , p 1 (-2) 0 , p 2 (-2) 0
          , p 3 (-2) 0 , p 3 (-1) 0 , p 4 (-1) 0 , p 5 (-1) 0
          , p 5 (-1) 0 , p 5 0 0 , p 4 2 0 , p 5 1 0
          , p 5 2 0 , p 4 3 0 , p 1 0 1 , p 1 1 1
          ]

main = do
  D.runFlareDrawing "controls1" "canvas1" $
    scene1 <$> intSlider "points" 3 10 8
           <*> numberSlider "offset" 0.0 (2.0 * pi) 0.01 0.0
           <*> numberSlider "rotate" (-pi / 6.0) (pi / 5.0) 0.01 0.0

  D.runFlareDrawing "controls2" "canvas2" $
    scene2 <$> numberSlider "Rotation" 0.0 (2.0 * pi) 0.1 0.0
           <*> lift animationFrame

  D.runFlareDrawing "controls3" "canvas3" $
    scene3 <$> numberSlider "Rotation" (-0.25 * pi) (0.25 * pi) 0.01 0.0
