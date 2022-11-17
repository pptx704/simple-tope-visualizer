{-# LANGUAGE OverloadedStrings #-}

-- | This module is applicable for 3D visualization only. Currently it handles rotation through KeyPress
-- however, layers are unchanged. This should be fixed.
module Visualize where

import           CodeWorld
import           CubeDrawings
import qualified Data.Text       as T
import qualified RSTT.Syntax.Abs as RSTT
import           SquareDrawings
import           TopeLayerData

data State = State {
      tope          :: RSTT.Tope
    , fShapes       :: [BasicShape3D]
    , rotation      :: (Double, Double)
    , squarePointer :: Int
}

updateWorld :: Event -> State -> State
updateWorld (KeyPress k) state@(State t s (x, y) n) = case k of
    "Up"    -> State t s (x + pi/8, y) n
    "Down"  -> State t s (x - pi/8, y) n
    "Left"  -> State t s (x, y - pi/8) n
    "Right" -> State t s (x, y + pi/8) n
    "W"     -> State t s (x, y) (max 0 (n-6))
    "S"     -> State t s (x, y) (min 23 (n+6))
    "D"     -> State t s (x, y) (min 23 (n+1))
    "A"     -> State t s (x, y) (max 0 (n-1))
    " "     -> State t' s' (x, y) n
    _       -> state
    where
        t' = if not $ isIncluded t addedTope
               then RSTT.TopeOr t addedTope
               else t
        addedTope = basicShapeTope ((drop 27 basicShapes3D) !! n)
        s' = filterShapes t' basicShapes3D
updateWorld (PointerPress (x, y)) s = trace (T.pack (show x ++ " " ++ show y)) $ s
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
drawWorld (State t s a n) = translated (-10) 0 $ renderBasicShapes3D (applyRotation a s)
    <> (background3D' . take 27) (applyRotation a basicShapes3D)
    <> (translated 0 8.5 . scaled 0.6 0.6 . lettering) (getTopeText t)
    <> translated 10 6 (sidePanel a n)

sidePanel :: (Double, Double) -> Int -> Picture
sidePanel a n = (drawList (n `div` 6) . convert 6 . drop 27 . applyRotation a) basicShapes3D
    where
        drawList _ []     = blank
        drawList 0 (i:is) = drawRow (n `mod` 6) i <> translated 0 (-3) (drawList (-1) is)
        drawList n' (i:is) = drawRow (-1) i <> translated 0 (-3) (drawList (n' - 1) is)
        drawRow _ []     = blank
        drawRow 0 (i:is) = scaled 0.35 0.35 ((render3Das2D i <> bg) <> colored (RGBA 0 0 1 0.3) (solidRectangle 1 1)) <> translated 3 0 (drawRow (-1) is)
        drawRow n' (i:is) = scaled 0.35 0.35 (render3Das2D i <> bg) <> translated 3 0 (drawRow (n'-1) is)
        bg = background3D' $ (applyRotation a .take 27) basicShapes3D

visualize :: RSTT.Tope -> IO()
visualize t = activityOf (State t (filterShapes t basicShapes3D) (0, 0) 0) updateWorld drawWorld
