{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CubeDrawings where

import           CodeWorld
import qualified RSTT.Syntax.Abs as RSTT
import           SquareDrawings
import           TopeLayerData
-- All symbols: ⊥ ⊤ ≤(t₁, t₂) ∧ ⊢ 𝟬 𝟭 ≡ ∨

type BasicShape3D = BasicShape (Double, Double, Double, Layer)

data Layer = Front | Middle | Back
  deriving Eq

getLayerColor :: Layer -> Color
getLayerColor Front = RGBA 0.76 0.44 1 0.4
getLayerColor Middle = RGBA 0.76 0.44 1 0.6
getLayerColor Back = RGBA 0.76 0.44 1 0.8

basicShapes3D :: [BasicShape3D]
basicShapes3D =
  [ BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟬 ∧ u ≡ 𝟬"    [(0, 0, 0, Back)]
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟬 ∧ u ≡ 𝟭"    [(0, 0, 1, Front)]
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟭 ∧ u ≡ 𝟬"    [(0, 1, 0, Back)]
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟭 ∧ u ≡ 𝟭"    [(0, 1, 1, Front)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟬 ∧ u ≡ 𝟬"    [(1, 0, 0, Back)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟬 ∧ u ≡ 𝟭"    [(1, 0, 1, Front)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟭 ∧ u ≡ 𝟬"    [(1, 1, 0, Back)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟭 ∧ u ≡ 𝟭"    [(1, 1, 1, Front)]

  , BasicShape "t ≡ 𝟬 ∧ u ≡ 𝟬"    [(0, 0, 0, Back), (1, 0, 0, Back)]
  , BasicShape "t ≡ 𝟭 ∧ u ≡ 𝟬"    [(0, 1, 0, Back), (1, 1, 0, Back)]
  , BasicShape "s ≡ 𝟬 ∧ u ≡ 𝟬"    [(0, 0, 0, Back), (0, 1, 0, Back)]
  , BasicShape "s ≡ 𝟭 ∧ u ≡ 𝟬"    [(1, 0, 0, Back), (1, 1, 0, Back)]
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟬"    [(0, 0, 0, Middle), (0, 0, 1, Middle)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟬"    [(1, 0, 0, Middle), (1, 0, 1, Middle)]
  , BasicShape "s ≡ 𝟬 ∧ t ≡ 𝟭"    [(0, 1, 0, Middle), (0, 1, 1, Middle)]
  , BasicShape "s ≡ 𝟭 ∧ t ≡ 𝟭"    [(1, 1, 0, Middle), (1, 1, 1, Middle)]
  , BasicShape "t ≡ 𝟬 ∧ u ≡ 𝟭"    [(0, 0, 1, Front), (1, 0, 1, Front)]
  , BasicShape "t ≡ 𝟭 ∧ u ≡ 𝟭"    [(0, 1, 1, Front), (1, 1, 1, Front)]
  , BasicShape "s ≡ 𝟬 ∧ u ≡ 𝟭"    [(0, 0, 1, Front), (0, 1, 1, Front)]
  , BasicShape "s ≡ 𝟭 ∧ u ≡ 𝟭"    [(1, 0, 1, Front), (1, 1, 1, Front)]
  ]

render3Das2D :: BasicShape3D -> Picture
render3Das2D (BasicShape _ shape) = case flatten3D shape of
  [(x, y, l)] -> colored (getLayerColor l) $ translated x y (solidCircle 0.15)
  [(x1, y1, l1), (x2, y2, l2)] -> if l1 == l2 then
    colored (getLayerColor l1) $ thickPolyline 0.1 [(x1, y1), (x2, y2)]
    else error "Cannot have edges of different layers"
  _ -> error "Not implemented yet"

flatten3D :: [(Double, Double, Double, Layer)] -> [(Double, Double, Layer)]
flatten3D = map flattenPoint
  where
    flattenPoint (x, y, z, l) = (x',y', l)
      where
        x' = 4*(x - z/2)
        y' = 4*(-y - z/2)

renderRow3D :: [BasicShape3D] -> Picture
renderRow3D (t:ts) = render3Das2D t <> translated 5 0 (renderRow3D ts)
renderRow3D [] = blank

mergeRow :: [BasicShape3D] -> Picture
mergeRow = foldMap render3Das2D

example3 :: Picture
example3 = mergeRow basicShapes3D