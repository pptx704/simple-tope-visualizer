{-# LANGUAGE OverloadedStrings #-}
module SquareDrawings where

import           CodeWorld
import           Data.List
import qualified Data.Text       as Text
import qualified Data.Text       as T
import           RSTT.Interpret  (convertTope)
import qualified RSTT.Interpret  as Interpret
import qualified RSTT.Syntax.Abs as RSTT
import           RSTT.Tope       (ppTope)
import qualified RSTT.Tope.Proof as Prover
import           TopeEquations
import           TopeLayerData

type BasicShape2D = BasicShape CodeWorld.Point

basicShapes2D :: [BasicShape2D]
basicShapes2D =
  [ BasicShape "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟬"    [(0, 0)]
  , BasicShape "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭"    [(0, 1)]
  , BasicShape "t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬"    [(1, 0)]
  , BasicShape "t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟭"    [(1, 1)]
  , BasicShape "t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭)"  [(1, 0), (1, 1)]
  , BasicShape "t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭)"  [(0, 0), (0, 1)]
  , BasicShape "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬"  [(0, 0), (1, 0)]
  , BasicShape "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭"  [(0, 1), (1, 1)]
  , BasicShape "t₁ ≡ t₂"            [(0, 0), (1, 1)]
  , BasicShape "≤(t₂, t₁)"          [(0, 0), (1, 0), (1, 1)]
  , BasicShape "≤(t₁, t₂)"          [(0, 0), (0, 1), (1, 1)]
  ]

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter((k==).length) $ subsequences ns

positionPoint :: [CodeWorld.Point] -> [CodeWorld.Point]
positionPoint []           = []
positionPoint ((a, b): ps) = (a*4, -b*4): positionPoint ps

renderBasicShape2D :: BasicShape2D -> Picture
renderBasicShape2D (BasicShape _tope points) =
  case positionPoint points of
    []                -> error "invalid basic shapes without points!"
    [(x, y)]          -> translated x y (solidCircle 0.15)
    path@[_, _]       -> thickPolyline 0.1 path
    path@[_, _, _]    -> solidPolygon path
    path@[_, _, _, _] -> blank -- Keeping blank because other triangles and points do their work
    --foldMap  solidPolygon $ combinations 3 path
    _                 -> error "cannot render in 3D or higher dimensions"

renderBasicShapes2D :: [BasicShape2D] -> Picture
renderBasicShapes2D = foldMap renderBasicShape2D

filterShapes :: RSTT.Tope -> [BasicShape a] -> [BasicShape a]
filterShapes tope = filter isIncluded
  where
    maxDepth = 20
    k = 1 -- depth of each DFS iteration
    rules = Prover.fromDefinedRules Prover.rulesLJE <> Prover.rulesLEQ
    isIncluded (BasicShape tope' _) =
      case Prover.proveWithBFSviaDFS' maxDepth k rules sequent of
        Nothing     -> False
        Just _proof -> True
      where
        sequent = Interpret.convertSequent
          (RSTT.Sequent RSTT.CubeContextEmpty (RSTT.TopeContextNonEmpty [tope']) tope)


renderTope :: RSTT.Tope -> [BasicShape a] -> ([BasicShape a] -> Picture) -> Picture
renderTope tope shapes renderer = renderer (filterShapes tope shapes)
    <> (translated 0 8.5 . scaled 0.6 0.6 . lettering) (T.pack $ (ppTope . convertTope) tope)

renderTope2D :: RSTT.Tope -> Picture
renderTope2D tope = renderTope tope basicShapes2D renderBasicShapes2D

background2D :: Picture
background2D = colored (light grey) (renderTope2D "⊤")


renderTope2DwithBackground :: Color -> RSTT.Tope -> Picture
renderTope2DwithBackground color tope
  = colored color (renderTope2D tope) <> background2D


-- | Functions to show all equations available
example1 :: Picture
example1 = renderRow get2DEquations


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
