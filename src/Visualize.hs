{-# LANGUAGE OverloadedStrings #-}

-- | This module is applicable for 3D visualization only. Currently it handles rotation through KeyPress
-- however, layers are unchanged. This should be fixed.
module Visualize where

import           CodeWorld
import           CubeDrawings
import           Data.List       ((\\))
import qualified Data.Text       as T
import qualified RSTT.Syntax.Abs as RSTT
import           SquareDrawings
import           TopeLayerData

data Rotation = Rotation
  { aroundX :: Double
  , aroundY :: Double
  }

zeroRotation :: Rotation
zeroRotation = Rotation 0 0

addRotation :: Rotation -> Rotation -> Rotation
addRotation (Rotation x1 y1) (Rotation x2 y2) = Rotation (x1 + x2) (y1 + y2)

data State = State
  { currentTope     :: (RSTT.Tope, [BasicShape3D])
  , currentRotation :: Rotation
  , squarePointer   :: Int
  }

prepareTope :: RSTT.Tope -> (RSTT.Tope, [BasicShape3D])
prepareTope tope = (tope, filterShapes tope basicShapes3D)

switchBasicShape :: BasicShape3D -> RSTT.Tope -> RSTT.Tope
switchBasicShape shape tope
  | tope `includes` shapeTope = removeTope shapeTope tope
  | otherwise = RSTT.TopeOr tope shapeTope
  where
    shapeTope = basicShapeTope shape

removeTopeUnsafe :: RSTT.Tope -> RSTT.Tope -> RSTT.Tope
removeTopeUnsafe shapeTope tope
  | shapeTope == tope = RSTT.TopeBottom
removeTopeUnsafe shapeTope (RSTT.TopeOr l r) =
  case (l', r') of
    (RSTT.TopeBottom, _) -> r'
    (_, RSTT.TopeBottom) -> l'
    _                    -> RSTT.TopeOr l' r'
  where
    l' = removeTopeUnsafe shapeTope l
    r' = removeTopeUnsafe shapeTope r
removeTopeUnsafe _ tope = tope

decompose3D :: RSTT.Tope -> [RSTT.Tope]
decompose3D tope = map basicShapeTope (filterShapes tope basicShapes3D)

topeOr :: [RSTT.Tope] -> RSTT.Tope
topeOr []           = RSTT.TopeBottom
topeOr [tope]       = tope
topeOr (tope:topes) = RSTT.TopeOr tope (topeOr topes)

removeTopeSafe3D :: RSTT.Tope -> RSTT.Tope -> RSTT.Tope
removeTopeSafe3D shapeTope tope = topeOr (topeComponents \\ shapeTopeComponents)
  where
    shapeTopeComponents = decompose3D shapeTope
    topeComponents = decompose3D tope

removeTope :: RSTT.Tope -> RSTT.Tope -> RSTT.Tope
removeTope shapeTope tope
  | tope' `includes` shapeTope = removeTopeSafe3D shapeTope tope
  | otherwise = tope'
  where
    tope' = removeTopeUnsafe shapeTope tope

rotateState :: Rotation -> State -> State
rotateState delta state = state
  { currentRotation = currentRotation state `addRotation` delta }

updateWorld :: Event -> State -> State
updateWorld (KeyPress key) state = case key of
  -- rotate 3D render
  "Up"    -> rotateState (Rotation rotationCoeff 0) state
  "Down"  -> rotateState (Rotation (-rotationCoeff) 0) state
  "Left"  -> rotateState (Rotation 0  (rotationCoeff)) state
  "Right" -> rotateState (Rotation 0 (-rotationCoeff)) state

  -- rotate 3D render
  "W"     -> state { squarePointer = max  0 (n - columnCount) }
  "S"     -> state { squarePointer = min facesAndVolumsCount (n + columnCount) }
  "D"     -> state { squarePointer = min facesAndVolumsCount (n + 1) }
  "A"     -> state { squarePointer = max  0 (n - 1) }

  -- add basic shape
  " "     -> state { currentTope = prepareTope (switchBasicShape currentBasicShape tope) }

  _       -> state
  where
    n = squarePointer state
    currentBasicShape = drop pointsAndEdgesCount basicShapes3D !! n
    (tope, _) = currentTope state
    pointsAndEdgesCount = 27
    facesAndVolumsCount = 23
    columnCount = 6
    rotationCoeff = pi/8
updateWorld _ s = s

applyRotationX :: Double -> [BasicShape3D] -> [BasicShape3D]
applyRotationX ang = map rotate
    where
        rotate (BasicShape t p) = BasicShape t (map (uncenter3D . rotateX . center3D) p)
        rotateX (x, y, z) = (x, (cos ang * y) - (sin ang * z), (sin ang * y) + (cos ang * z))

applyRotationY :: Double -> [BasicShape3D] -> [BasicShape3D]
applyRotationY ang = map rotate
    where
        rotate (BasicShape t p) = BasicShape t (map (uncenter3D . rotateY . center3D) p)
        rotateY (x, y, z) = ((cos ang * x) + (sin ang * z), y, (cos ang * z) - (sin ang * x))

applyRotation :: Rotation -> [BasicShape3D] -> [BasicShape3D]
applyRotation (Rotation x y) = applyRotationX x . applyRotationY y

drawWorld :: State -> Picture
drawWorld (State (tope, topeComponents) rotation currentIndex) = translated (-10) 0 $ pictures
  [ renderBasicShapes3D (applyRotation rotation topeComponents)
  , background3D' pointsAndEdges
  , translated 0 8.5 (scaled 0.6 0.6 (lettering (getTopeText tope)))
  , translated 10 6 (sidePanel rotation currentIndex)
  ]
  where
    rotatedBasicShapes3D = applyRotation rotation basicShapes3D
    (pointsAndEdges, _facesAndVolumes) = splitByDimension rotatedBasicShapes3D

sidePanel :: Rotation -> Int -> Picture
sidePanel rotation n = pictures
  [ translated x (-y) (scaled 0.35 0.35 (render3Das2D shape <> background)
    <> if i == n then selectionBox else blank)
  | (i, shape) <- zip [0..] facesAndVolumes
  , let column = i `mod` rowSize
  , let row    = i `div` rowSize
  , let x = cellSize * fromIntegral column
  , let y = cellSize * fromIntegral row
  ]
  where
    selectionBox = colored red (rectangle cellSize cellSize)
    rotatedBasicShapes3D = applyRotation rotation basicShapes3D
    (pointsAndEdges, facesAndVolumes) = splitByDimension rotatedBasicShapes3D
    background = background3D' pointsAndEdges
    rowSize = 6
    cellSize = 3

visualize :: RSTT.Tope -> IO ()
visualize t = activityOf (State (prepareTope t) zeroRotation 0) updateWorld drawWorld
