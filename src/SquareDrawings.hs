{-# LANGUAGE OverloadedStrings #-}
module SquareDrawings where

import           CodeWorld
import           Data.String     (IsString (..))
import qualified RSTT.Interpret  as Interpret
import qualified RSTT.Syntax.Abs as RSTT
import qualified RSTT.Syntax.Par as RSTT
import qualified RSTT.Tope.Proof as Prover
import           TopeLayerData

instance IsString RSTT.Tope where
  fromString = unsafeFromRight . RSTT.pTope . RSTT.myLexer
    where
      unsafeFromRight (Right x)  = x
      unsafeFromRight (Left msg) = error msg

data BasicShape = BasicShape
  { basicShapeTope   :: RSTT.Tope
  , basicShapePoints :: [CodeWorld.Point]
  }

basicShapes2D :: [BasicShape]
basicShapes2D =
  [ BasicShape "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟬"  [(0, 0)]
  , BasicShape "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭"  [(0, 1)]
  , BasicShape "t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬"  [(1, 0)]
  , BasicShape "t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟭"  [(1, 1)]
  , BasicShape "≤(t₂, t₁)"        [(0, 0), (1, 0), (1, 1)]
  -- TODO: add more basic shapes
  ]

renderBasicShape2D :: BasicShape -> Picture
renderBasicShape2D (BasicShape _tope points) =
  case points of
    []             -> error "invalid basic shapes without points!"
    [(x, y)]       -> translated x y (solidCircle 0.15)
    path@[_, _]    -> thickPolyline 0.1 path
    path@[_, _, _] -> solidPolygon path
    _              -> error "cannot render in 3D or higher dimensions"

renderBasicShapes2D :: [BasicShape] -> Picture
renderBasicShapes2D = foldMap renderBasicShape2D

renderTope2D :: RSTT.Tope -> Picture
renderTope2D tope = renderBasicShapes2D (filter isIncluded basicShapes2D)
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

background :: Picture
background = colored grey (renderTope2D "⊤")

renderTope2DwithBackground :: Color -> RSTT.Tope -> Picture
renderTope2DwithBackground color tope
  = colored color (renderTope2D tope)

example1 :: Picture
example1 = renderTope2DwithBackground blue "≤(t₂, t₁) ∧ t₂ ≡ 𝟬"
