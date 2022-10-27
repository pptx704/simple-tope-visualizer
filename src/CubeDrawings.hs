{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CubeDrawings where

import           CodeWorld
import qualified RSTT.Syntax.Abs as RSTT
import           SquareDrawings
import           TopeLayerData
-- All symbols: ⊥ ⊤ ≤(t₁, t₂) ∧ ⊢ 𝟬 𝟭 ≡ ∨

type BasicShape3D = BasicShape (Double, Double, Double)


getLayerColor :: Layer -> Color
getLayerColor Front   = RGBA 0.86 0.54 0.7 0.6
getLayerColor Back  = RGBA 0.66 0.34 0.9 0.6
getLayerColor Middle = RGBA 0.7 0.7 0 0.6
getLayerColor None   = RGBA 0 0 0 0

basicShapes3D :: [BasicShape3D]
basicShapes3D =
  [ BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟬 ∧ u ≡ 𝟬"    [(0, 0, 0)]             Back
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟬 ∧ u ≡ 𝟭"    [(0, 0, 1)]             Front
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟭 ∧ u ≡ 𝟬"    [(0, 1, 0)]             Back
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟭 ∧ u ≡ 𝟭"    [(0, 1, 1)]             Front
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟬 ∧ u ≡ 𝟬"    [(1, 0, 0)]             Back
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟬 ∧ u ≡ 𝟭"    [(1, 0, 1)]             Front
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟭 ∧ u ≡ 𝟬"    [(1, 1, 0)]             Back
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟭 ∧ u ≡ 𝟭"    [(1, 1, 1)]             Front
  , BasicShape "t ≡ 𝟬 ∧ u ≡ 𝟬"            [(0, 0, 0), (1, 0, 0)]  Back
  , BasicShape "t ≡ 𝟭 ∧ u ≡ 𝟬"            [(0, 1, 0), (1, 1, 0)]  Back
  , BasicShape "s ≡ 𝟬 ∧ u ≡ 𝟬"            [(0, 0, 0), (0, 1, 0)]  Back
  , BasicShape "s ≡ 𝟭 ∧ u ≡ 𝟬"            [(1, 0, 0), (1, 1, 0)]  Back
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟬"            [(0, 0, 0), (0, 0, 1)]  Middle
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟬"            [(1, 0, 0), (1, 0, 1)]  Middle
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟭"            [(0, 1, 0), (0, 1, 1)]  Middle
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟭"            [(1, 1, 0), (1, 1, 1)]  Middle
  , BasicShape "t ≡ 𝟬 ∧ u ≡ 𝟭"            [(0, 0, 1), (1, 0, 1)]  Front
  , BasicShape "t ≡ 𝟭 ∧ u ≡ 𝟭"            [(0, 1, 1), (1, 1, 1)]  Front
  , BasicShape "s ≡ 𝟬 ∧ u ≡ 𝟭"            [(0, 0, 1), (0, 1, 1)]  Front
  , BasicShape "s ≡ 𝟭 ∧ u ≡ 𝟭"            [(1, 0, 1), (1, 1, 1)]  Front
  , BasicShape "s ≡ t ∧ u ≡ 𝟬"            [(0, 0, 0), (1, 1, 0)]  Back
  , BasicShape "s ≡ t ∧ u ≡ 𝟭"            [(0, 0, 1), (1, 1, 1)]  Front
  , BasicShape "s ≡ 𝟬 ∧ t ≡ u"            [(0, 0, 0), (0, 1, 1)]  Middle
  , BasicShape "s ≡ 𝟭 ∧ t ≡ u"            [(1, 0, 0), (1, 1, 1)]  Middle
  , BasicShape "s ≡ u ∧ t ≡ 𝟬"            [(0, 0, 0), (1, 0, 1)]  Middle
  , BasicShape "s ≡ u ∧ t ≡ 𝟭"            [(0, 1, 0), (1, 1, 1)]  Middle
  , BasicShape "s ≡ t ∧ t ≡ u"            [(0, 0, 0), (1, 1, 1)]  Middle
  


  -- The idea is, every basic shape (point, line, triangle) is either on one of the faces
  -- if not faces (i.e. line (0,0,0) -> (1,1,1)) the shapes still remain 'inside' the 3D cube
  -- using these information, I think it is possible to produce the 3D diagram properly

  -- Another way is to use the same alpha for for shapes, but maintain which one to
  -- render on top of another. However, I am skeptic about that implementation
  ]

render3Das2D :: BasicShape3D -> Picture
render3Das2D (BasicShape _ shape l) = case flatten3D shape of
  [(x, y)] -> colored (getLayerColor l) $ translated x y (solidCircle 0.15)
  [(x1, y1), (x2, y2)] ->
    colored (getLayerColor l) $ thickPolyline 0.1 [(x1, y1), (x2, y2)]
  _ -> error "Not implemented yet"

flatten3D :: [(Double, Double, Double)] -> [(Double, Double)]
flatten3D = map flattenPoint
  where
    flattenPoint (x, y, z) = (x',y')
      where
        x' = 4*(x - z/2) - z/2
        y' = 4*(-y - z/2)

renderRow3D :: [BasicShape3D] -> Picture
renderRow3D (t:ts) = render3Das2D t <> translated 5 0 (renderRow3D ts)
renderRow3D []     = blank

mergeRow :: [BasicShape3D] -> Picture
mergeRow = foldMap render3Das2D

example3 :: Picture
example3 = mergeRow basicShapes3D
