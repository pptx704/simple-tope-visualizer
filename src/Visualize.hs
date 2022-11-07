{-# LANGUAGE OverloadedStrings #-}

-- | This module is applicable for 3D visualization only. Currently it handles rotation through KeyPress
-- however, layers are unchanged. This should be fixed.
module Visualize where

import CodeWorld
import CubeDrawings
import SquareDrawings
import TopeLayerData
import qualified RSTT.Syntax.Abs as RSTT

data State = State {
      tope :: RSTT.Tope
    , shapes :: [BasicShape3D]
}

updateWorld :: Event -> State -> State
updateWorld (KeyPress k) state@(State t s) = case k of
    "Up" -> State t (applyRotationX (-pi/8) s)
    "Down" -> State t (applyRotationX (pi/8) s)    
    "Left" -> State t (applyRotationY (-pi/8) s)
    "Right" -> State t (applyRotationY (pi/8) s)
    _ -> state
updateWorld _ s = s

applyRotationX :: Double -> [BasicShape3D] -> [BasicShape3D]
applyRotationX ang = map rotate
    where
        rotate (BasicShape t p l) = BasicShape t (map rotateX p) l
        rotateX (x, y, z) = (x, (cos ang * y) - (sin ang * z), (sin ang * y) + (cos ang * z))

applyRotationY :: Double -> [BasicShape3D] -> [BasicShape3D]
applyRotationY ang = map rotate
    where
        rotate (BasicShape t p l) = BasicShape t (map rotateY p) l
        rotateY (x, y, z) = ((cos ang * x) + (sin ang * z), y, (cos ang * z) - (sin ang * x))


drawWorld :: State -> Picture
drawWorld (State t s) = renderTope t s renderBasicShapes3D <> (background3D' . take 27) s

visualize :: RSTT.Tope -> IO()
visualize t = activityOf (State t basicShapes3D) updateWorld drawWorld
