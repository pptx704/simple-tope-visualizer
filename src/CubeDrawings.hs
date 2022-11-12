{-# LANGUAGE OverloadedStrings #-}

module CubeDrawings where

import           CodeWorld
import qualified RSTT.Syntax.Abs as RSTT
import           SquareDrawings
import           TopeLayerData

-- All symbols: ⊥ ⊤ ≤(t₁, t₂) ∧ ⊢ 𝟬 𝟭 ≡ ∨

type BasicShape3D = BasicShape (Double, Double, Double)

basicShapes3D :: [BasicShape3D]
basicShapes3D =
  [
    BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟬 ∧ u ≡ 𝟬"    [(0, 0, 0)]
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟬 ∧ u ≡ 𝟭"    [(0, 0, 1)]
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟭 ∧ u ≡ 𝟬"    [(0, 1, 0)]
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟭 ∧ u ≡ 𝟭"    [(0, 1, 1)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟬 ∧ u ≡ 𝟬"    [(1, 0, 0)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟬 ∧ u ≡ 𝟭"    [(1, 0, 1)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟭 ∧ u ≡ 𝟬"    [(1, 1, 0)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟭 ∧ u ≡ 𝟭"    [(1, 1, 1)]
  , BasicShape "t ≡ 𝟬 ∧ u ≡ 𝟬"            [(0, 0, 0), (1, 0, 0)]
  , BasicShape "t ≡ 𝟭 ∧ u ≡ 𝟬"            [(0, 1, 0), (1, 1, 0)]
  , BasicShape "s ≡ 𝟬 ∧ u ≡ 𝟬"            [(0, 0, 0), (0, 1, 0)]
  , BasicShape "s ≡ 𝟭 ∧ u ≡ 𝟬"            [(1, 0, 0), (1, 1, 0)]
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟬"            [(0, 0, 0), (0, 0, 1)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟬"            [(1, 0, 0), (1, 0, 1)]
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟭"            [(0, 1, 0), (0, 1, 1)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟭"            [(1, 1, 0), (1, 1, 1)]
  , BasicShape "t ≡ 𝟬 ∧ u ≡ 𝟭"            [(0, 0, 1), (1, 0, 1)]
  , BasicShape "t ≡ 𝟭 ∧ u ≡ 𝟭"            [(0, 1, 1), (1, 1, 1)]
  , BasicShape "s ≡ 𝟬 ∧ u ≡ 𝟭"            [(0, 0, 1), (0, 1, 1)]
  , BasicShape "s ≡ 𝟭 ∧ u ≡ 𝟭"            [(1, 0, 1), (1, 1, 1)]
  , BasicShape "s ≡ t ∧ u ≡ 𝟬"            [(0, 0, 0), (1, 1, 0)]
  , BasicShape "s ≡ t ∧ u ≡ 𝟭"            [(0, 0, 1), (1, 1, 1)]
  , BasicShape "s ≡ 𝟬 ∧ t ≡ u"            [(0, 0, 0), (0, 1, 1)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ u"            [(1, 0, 0), (1, 1, 1)]
  , BasicShape "s ≡ u ∧ t ≡ 𝟬"            [(0, 0, 0), (1, 0, 1)]
  , BasicShape "s ≡ u ∧ t ≡ 𝟭"            [(0, 1, 0), (1, 1, 1)]
  , BasicShape "s ≡ t ∧ t ≡ u"            [(0, 0, 0), (1, 1, 1)]
  , BasicShape "s ≡ 𝟬 ∧ ≤(u, t)"          [(0, 0, 0), (0, 1, 0), (0, 1, 1)] -- 0
  , BasicShape "s ≡ 𝟬 ∧ ≤(t, u)"          [(0, 0, 0), (0, 0, 1), (0, 1, 1)] -- 1
  , BasicShape "s ≡ 𝟭 ∧ ≤(u, t)"          [(1, 0, 0), (1, 1, 0), (1, 1, 1)] -- 2
  , BasicShape "s ≡ 𝟭 ∧ ≤(t, u)"          [(1, 0, 0), (1, 0, 1), (1, 1, 1)] -- 3
  , BasicShape "u ≡ 𝟬 ∧ ≤(t, s)"          [(0, 0, 0), (1, 0, 0), (1, 1, 0)] -- 6
  , BasicShape "u ≡ 𝟬 ∧ ≤(s, t)"          [(0, 0, 0), (0, 1, 0), (1, 1, 0)] -- 7
  , BasicShape "u ≡ 𝟭 ∧ ≤(t, s)"          [(0, 0, 1), (1, 0, 1), (1, 1, 1)] -- 4
  , BasicShape "u ≡ 𝟭 ∧ ≤(s, t)"          [(0, 0, 1), (0, 1, 1), (1, 1, 1)] -- 5
  , BasicShape "t ≡ 𝟬 ∧ ≤(s, u)"          [(0, 0, 0), (0, 0, 1), (1, 0, 1)] -- 8
  , BasicShape "t ≡ 𝟬 ∧ ≤(u, s)"          [(0, 0, 0), (1, 0, 0), (1, 0, 1)] -- 10
  , BasicShape "t ≡ 𝟭 ∧ ≤(s, u)"          [(0, 1, 0), (0, 1, 1), (1, 1, 1)] -- 9
  , BasicShape "t ≡ 𝟭 ∧ ≤(u, s)"          [(0, 1, 0), (1, 1, 0), (1, 1, 1)] -- 11
  , BasicShape "s ≡ u ∧ ≤(u, t)"          [(0, 0, 0), (0, 1, 0), (1, 1, 1)] -- 12
  , BasicShape "s ≡ u ∧ ≤(t, u)"          [(0, 0, 0), (1, 0, 1), (1, 1, 1)] -- 13
  , BasicShape "t ≡ u ∧ ≤(t, s)"          [(0, 0, 0), (1, 0, 0), (1, 1, 1)] -- 14
  , BasicShape "t ≡ u ∧ ≤(s, t)"          [(0, 0, 0), (0, 1, 1), (1, 1, 1)] -- 15
  , BasicShape "s ≡ t ∧ ≤(t, u)"          [(0, 0, 0), (0, 0, 1), (1, 1, 1)] -- 16
  , BasicShape "s ≡ t ∧ ≤(u, t)"          [(0, 0, 0), (1, 1, 0), (1, 1, 1)] -- 17
  , BasicShape "≤(u, t) ∧ ≤(s, u)"        [(0, 0, 0), (0, 1, 0), (0, 1, 1), (1, 1 ,1)] -- Triangle 0, 9, 12, 15
  , BasicShape "≤(u, s) ∧ ≤(t, u)"        [(0, 0, 0), (1, 0, 1), (1, 0, 0), (1, 1 ,1)] -- Triangle 3, 10, 13, 14
  , BasicShape "≤(s, u) ∧ ≤(t, s)"        [(0, 0, 0), (0, 0, 1), (1, 0, 1), (1, 1 ,1)] -- Triangle 4, 8, 13, 16
  , BasicShape "≤(s, t) ∧ ≤(u, s)"        [(0, 0, 0), (0, 1, 0), (1, 1, 0), (1, 1 ,1)] -- Triangle 7, 11, 12, 17
  , BasicShape "≤(s, t) ∧ ≤(t, u)"        [(0, 0, 0), (1, 0, 0), (1, 1, 0), (1, 1 ,1)] -- Triangle 2, 6, 14, 17
  , BasicShape "≤(u, t) ∧ ≤(t, s)"        [(0, 0, 0), (0, 0, 1), (0, 1, 1), (1, 1 ,1)] -- Triangle 1, 5, 15, 16
  ]


render3Das2D :: BasicShape3D -> Picture
render3Das2D shp@(BasicShape _ _) = (colored col . renderBasicShape2D . from3D) shp
  where
    col = RGBA 1 0 0 0.3
    from3D (BasicShape t shape) = BasicShape t (flatten3D shape)

renderBasicShapes3D :: [BasicShape3D] -> Picture
renderBasicShapes3D = foldMap render3Das2D

renderTope3D :: RSTT.Tope -> Picture
renderTope3D tope = renderTope tope basicShapes3D renderBasicShapes3D

renderTope3DwithBackground :: RSTT.Tope -> Picture
renderTope3DwithBackground t = renderTope3D t <> background3D

flatten3D :: [(Double, Double, Double)] -> [(Double, Double)]
flatten3D = map flattenPoint
  where
    flattenPoint (x, y, z) = (x',y')
      where
        x' = x - z/2 -- -z/8 is used to rotate the cube a bit for better representations
        y' = y + z/2

background3D' :: [BasicShape3D] -> Picture
background3D' shapes = colored black $ renderBasicShapes3D shapes


background3D :: Picture
background3D = background3D' $ take 27 basicShapes3D
