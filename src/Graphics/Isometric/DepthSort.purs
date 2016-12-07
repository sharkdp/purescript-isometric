module Graphics.Isometric.DepthSort
  ( depthSort
  ) where

import Prelude

import Data.List.ThreeOrMore (toList)
import Data.Maybe (fromJust)
import Data.Foldable (foldMap, maximum, minimum)
import Data.Graph (Graph, unfoldGraph, topologicalSort)
import Data.List (List, filter, length, (..), zipWith, singleton, concat)
import Partial.Unsafe (unsafePartial)

import Graphics.Isometric (Shape, Scene(Fill, Many))

data Vertex = Vertex Int Scene

instance eqVertex :: Eq Vertex where
  eq (Vertex i _) (Vertex j _) = eq i j

instance ordVertex :: Ord Vertex where
  compare (Vertex i _) (Vertex j _) = compare i j

type DepthGraph = Graph Vertex Vertex

-- | Remove all `Many` constructors from a scene.
flatten :: Scene -> List Scene
flatten (Many ss) = ss >>= flatten
flatten s = singleton s

type Bounds =
  { minX :: Number
  , maxX :: Number
  , minY :: Number
  , maxY :: Number
  , minZ :: Number
  , maxZ :: Number }

-- | Calculate the bounds of a 3D shape.
bounds :: Shape -> Bounds
bounds shape = { minX: min' cX, maxX: max' cX
               , minY: min' cY, maxY: max' cY
               , minZ: min' cZ, maxZ: max' cZ }
  where
    coords = concat (toList <$> shape)
    cX = _.x <$> coords
    cY = _.y <$> coords
    cZ = _.z <$> coords
    min' = unsafePartial (fromJust <<< minimum)
    max' = unsafePartial (fromJust <<< maximum)

-- | Check if two vertices/shapes overlap and determine the depth ordering.
isBehind :: Partial => Vertex -> Vertex -> Boolean
isBehind (Vertex _ (Fill _ s1)) (Vertex _ (Fill _ s2)) = decide
  where
    b1 = bounds s1
    b2 = bounds s2
    decide
      | b1.maxX <= b2.minX = true
      | b2.maxX <= b1.minX = false
      | b1.maxY <= b2.minY = true
      | b2.maxY <= b1.minY = false
      | b1.maxZ <= b2.minZ = true
      | b2.maxZ <= b1.minZ = false
      | otherwise = true

-- | Transform a 3D scene to a graph where the vertices are single objects
-- | and the edges are `isBehind` relations.
toGraph :: Scene -> DepthGraph
toGraph scene = unfoldGraph vertices id edges
  where
    addKey scene key = Vertex key scene
    addKeys list = zipWith addKey list (0 .. (length list - 1))

    vertices = addKeys (flatten scene)

    edges i = filter (\j -> unsafePartial (i `isBehind` j)) vertices

-- | Transform a (sorted) list of vertices/shapes back to a 3D scene.
toScene :: List Vertex -> Scene
toScene vertices = foldMap dropIndex vertices
  where dropIndex (Vertex _ scene) = scene

-- | Sort the objects in a scene by depth such that they will be rendered
-- | correctly.
depthSort :: Scene -> Scene
depthSort = toGraph >>> topologicalSort >>> toScene
