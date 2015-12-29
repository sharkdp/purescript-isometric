module Test.Main where

import Prelude

import Data.Array
import Data.Int
import Data.Foldable

import Graphics.Isometric

import Math (sin, cos, pi)

import Signal.DOM

import Flare
import Flare.Drawing as D

scene n omega angle =
  D.translate 250.0 120.0 $
    renderScene angle { x: -4.0, y: -1.0, z: 3.0 } $
      fold do
        i <- 0..n
        j <- 0..n
        let x = toNumber i / toNumber n
            y = toNumber j / toNumber n
            p = { x: dl * toNumber i, y: dl * toNumber j, z: 0.0 }
            h = 2.0 * dl + 1.5 * dl * sin (2.0 * omega * x) * cos (omega * y)
        return $ filled (D.hsl (300.0 * x) 0.5 0.5) (prism p w w h)
  where dl = 230.0 / toNumber n
        w = 0.9 * dl

main = do
  D.runFlareDrawing "controls" "canvas" $
    scene <$> intSlider "points" 3 15 8
          <*> numberSlider "omega" (0.5 * pi) (1.5 * pi) 0.01 (0.5 * pi)
          <*> numberSlider "angle" 0.0 (pi / 2.0) 0.01 isometricAngle
