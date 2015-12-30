module Test.Main where

import Prelude

import Data.Array
import Data.Int
import Data.Foldable

import Graphics.Isometric

import Math (sin, cos, pi)

import Flare
import Flare.Drawing as D

scene :: Int -> Number -> Angle -> D.Drawing
scene n offset angle =
  D.translate 250.0 200.0 $
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

main = do
  D.runFlareDrawing "controls" "canvas" $
    scene <$> intSlider "points" 3 10 8
          <*> numberSlider "offset" 0.0 (2.0 * pi) 0.01 0.0
          <*> numberSlider "rotate" (-pi / 6.0) (pi / 5.0) 0.01 0.0
