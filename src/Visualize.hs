{-# LANGUAGE OverloadedStrings #-}

-- | This module is applicable for 3D visualization only. Currently it handles rotation through KeyPress
-- however, layers are unchanged. This should be fixed.
module Visualize where

import           CodeWorld
import           CubeDrawings
import qualified RSTT.Syntax.Abs as RSTT
import           SquareDrawings
import           TopeLayerData

data State = State {
      tope     :: RSTT.Tope
    , fShapes  :: [BasicShape3D]
    , rotation :: (Double, Double)
}

updateWorld :: Event -> State -> State
updateWorld (KeyPress k) state@(State t s (x, y)) = case k of
    "Up"    -> State t s (x + pi/8, y)
    "Down"  -> State t s (x - pi/8, y)
    "Left"  -> State t s (x, y - pi/8)
    "Right" -> State t s (x, y + pi/8)
    _       -> state
updateWorld _ s = s

applyRotationX :: Double -> [BasicShape3D] -> [BasicShape3D]
applyRotationX ang = map rotate
    where
        rotate (BasicShape t p) = BasicShape t (map rotateX p)
        rotateX (x, y, z) = (x, (cos ang * y) - (sin ang * z), (sin ang * y) + (cos ang * z))

applyRotationY :: Double -> [BasicShape3D] -> [BasicShape3D]
applyRotationY ang = map rotate
    where
        rotate (BasicShape t p) = BasicShape t (map rotateY p)
        rotateY (x, y, z) = ((cos ang * x) + (sin ang * z), y, (cos ang * z) - (sin ang * x))

applyRotation :: (Double, Double) -> [BasicShape3D] -> [BasicShape3D]
applyRotation (x, y) = applyRotationX x . applyRotationY y

drawWorld :: State -> Picture
drawWorld (State t s a) = translated (-10) 0 $ renderTope t (applyRotation a s) renderBasicShapes3D
    <> (background3D' . take 27) (applyRotation a basicShapes3D)

visualize :: RSTT.Tope -> IO()
visualize t = activityOf (State t (filterShapes t basicShapes3D) (0, 0)) updateWorld drawWorld
