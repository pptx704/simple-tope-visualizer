{-# LANGUAGE OverloadedStrings #-}

module SquareDrawings where

import           CodeWorld
import           Data.List
import qualified Data.Text       as Text
import           Debug.Trace
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
    -- for tetrahedrons, picture is not needed since the triangles will form it
    -- However, something needs to be done to show the topes on side panel
    [a, b, c, d] -> foldMap solidPolygon [[b,c,d], [a,c,d], [a,b,d], [a,b,c]]
    --path@[_, _, _, _] -> blank
    _                 -> blank-- error "cannot render in 3D or higher dimensions"

renderBasicShapes2D :: [BasicShape2D] -> Picture
renderBasicShapes2D = foldMap renderBasicShape2D

filterShapes :: RSTT.Tope -> [BasicShape a] -> [BasicShape a]
filterShapes tope = filter isIncluded'
    where
        isIncluded' (BasicShape tope' _) = tope `includes` tope'

includes :: RSTT.Tope -> RSTT.Tope -> Bool
tope `includes` tope' =
    case Prover.proveWithBFSviaDFS' maxDepth k rules sequent of
      Nothing -> False
      Just _  -> True
     where
      sequent = Interpret.convertSequent
          (RSTT.Sequent RSTT.CubeContextEmpty (RSTT.TopeContextNonEmpty [tope']) tope)
      maxDepth = 20
      k = 1
      rules = Prover.fromDefinedRules Prover.rulesLJE <> Prover.rulesLEQ

renderTope :: RSTT.Tope -> [BasicShape a] -> ([BasicShape a] -> Picture) -> Picture
renderTope tope shapes renderer = renderer (filterShapes tope shapes)

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
