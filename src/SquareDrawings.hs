{-# LANGUAGE OverloadedStrings #-}
module SquareDrawings where

import           CodeWorld
import qualified Data.Text       as Text
import qualified RSTT.Interpret  as Interpret
import qualified RSTT.Syntax.Abs as RSTT
import           RSTT.Tope       (ppTope)
import qualified RSTT.Tope.Proof as Prover
import           TopeEquations
import           TopeLayerData

type BasicShape2D = BasicShape CodeWorld.Point

basicShapes2D :: [BasicShape2D]
basicShapes2D =
  [ BasicShape "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟬"    [(0, 0)] None
  , BasicShape "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭"    [(0, 1)] None
  , BasicShape "t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬"    [(1, 0)] None
  , BasicShape "t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟭"    [(1, 1)] None
  , BasicShape "t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭)"  [(1, 0), (1, 1)] None
  , BasicShape "t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭)"  [(0, 0), (0, 1)] None
  , BasicShape "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬"  [(0, 0), (1, 0)] None
  , BasicShape "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭"  [(0, 1), (1, 1)] None
  , BasicShape "t₁ ≡ t₂"            [(0, 0), (1, 1)] None
  , BasicShape "≤(t₂, t₁)"          [(0, 0), (1, 0), (1, 1)] None
  , BasicShape "≤(t₁, t₂)"          [(0, 0), (0, 1), (1, 1)] None
  ]

magnifyPath :: [CodeWorld.Point] -> [CodeWorld.Point]
magnifyPath []           = []
magnifyPath ((a, b): ps) = (a*4, -b*4): magnifyPath ps

renderBasicShape2D :: BasicShape2D -> Picture
renderBasicShape2D (BasicShape _tope points _) =
  case magnifyPath points of
    []             -> error "invalid basic shapes without points!"
    [(x, y)]       -> translated x y (solidCircle 0.15)
    path@[_, _]    -> thickPolyline 0.1 path
    path@[_, _, _] -> solidPolygon path
    _              -> error "cannot render in 3D or higher dimensions"

renderBasicShapes2D :: [BasicShape2D] -> Picture
renderBasicShapes2D = foldMap renderBasicShape2D

renderTope2D :: RSTT.Tope -> Picture
renderTope2D tope = renderBasicShapes2D (filter isIncluded basicShapes2D)
  where
    maxDepth = 20
    k = 1 -- depth of each DFS iteration
    rules = Prover.fromDefinedRules Prover.rulesLJE <> Prover.rulesLEQ
    isIncluded (BasicShape tope' _ _) =
      case Prover.proveWithBFSviaDFS' maxDepth k rules sequent of
        Nothing     -> False
        Just _proof -> True
      where
        sequent = Interpret.convertSequent
          (RSTT.Sequent RSTT.CubeContextEmpty (RSTT.TopeContextNonEmpty [tope']) tope)

background :: Picture
background = colored (light grey) (renderTope2D "⊤")


renderTope2DwithBackground :: Color -> RSTT.Tope -> Picture
renderTope2DwithBackground color tope
  = colored color (renderTope2D tope) <> background


-- | Functions to show all equations available
example1 :: Picture
example1 = (renderRow . take 10) get2DEquations

example2 :: Picture
example2 = renderTope2DwithBackground red tope' <> translated 0 (-5) (lettering $ getTopeText tope')
  where
    tope' = get2DEquations !! 5

getTopeText :: RSTT.Tope -> Text.Text
getTopeText = Text.pack . ppTope . Interpret.convertTope

convert :: Int -> [a] -> [[a]]
convert _ [] = []
convert n xs = take n xs : convert n (drop n xs)

renderRow :: [RSTT.Tope] -> Picture
renderRow (t:ts) = renderTope2DwithBackground red t <> translated 5 0 (renderRow ts)
renderRow [] = blank

renderList :: [[RSTT.Tope]] -> Picture
renderList (t:ts) = renderRow t <> translated 0 (-5) (renderList ts)
renderList []     = blank
