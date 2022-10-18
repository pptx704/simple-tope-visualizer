{-# LANGUAGE OverloadedStrings #-}

module TopeEquations where

import TopeLayerData ()
import qualified RSTT.Syntax.Abs as RSTT
-- All symbols: ⊥ ⊤ ≤(t₁, t₂) ∧ ⊢ 𝟬 𝟭 ≡ ∨

-- | There are 14 more equations than possible. Need to take the impostors out
get2DEquations :: [RSTT.Tope]
get2DEquations = [
        "⊥" -- Empty
        -- basic shapes
        , "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟬" -- (0, 0)
        , "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭" -- (0, 1)
        , "t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬" -- (1, 0)
        , "t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟭" -- (1, 1)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭" -- Top edge (Problem (0,1))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬" -- Bottom edge (Problem (0, 0))
        , "t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭)" -- Left edge (Problem (0, 0))
        , "t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭)" -- Right edge (Problem (1, 0))
        , "t₁ ≡ t₂" -- Diagonal edge
        , "≤(t₁, t₂)" -- Top triangle (Problem: Doesn't show (0,1) point)
        , "≤(t₂, t₁)" -- Bottom triangle (Problem: Doesn't show (1,1) point)
        -- 2 Points
        , "t₁ ≡ 𝟬 ∧ (t₂ ≡ 𝟬 ∨ t₂ ≡ 𝟭)" -- (0,0) and (0,1)
        , "t₁ ≡ 𝟭 ∧ (t₂ ≡ 𝟬 ∨ t₂ ≡ 𝟭)" -- (1,0) and (1,1)
        , "(t₁ ≡ 𝟬 ∨ t₁ ≡ 𝟭) ∧ t₂ ≡ 𝟬" -- (0,0) and (1,0)
        , "(t₁ ≡ 𝟬 ∨ t₁ ≡ 𝟭) ∧ t₂ ≡ 𝟭" -- (0,1) and (1,1)
        , "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟭" -- (0,0) and (1,1)
        , "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬" -- (0, 1) and (1, 0)
        -- 3 Points
        , "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟭" -- (0,1), (0, 1) and (1,1)
        , "t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟭" -- (0,1), (1, 0) and (1,1)
        -- all 4 points
        , "(t₁ ≡ 𝟬 ∨ t₁ ≡ 𝟭) ∧ (t₂ ≡ 𝟬 ∨ t₂ ≡ 𝟭)"
        -- 2 edges
        , "(t₁ ≡ 𝟬 ∨ t₁ ≡ 𝟭) ∧ ≤(t₂, 𝟭)" -- Right edge and left edge (Problem doesn't show points with t2 = 0)
        , "≤(t₁, 𝟭) ∧ (t₂ ≡ 𝟬 ∨ t₂ ≡ 𝟭)" -- Top edge and bottm edge (Problem doesn't show points with t1 = 0)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭)"-- Top edge and left edge (Problem (0, 0))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭)" -- Top edge and right edge (Problem (0, 1))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭)" -- Bottom edge and left edge (Problem (0, 0))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭)" -- Bottom edge and right edge (Problem point (0,0))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ t₂" -- Top and diagonal (Problem (0, 1))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ t₂" -- Bottom and diagonal
        , "t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ t₂" -- Left and diagonal
        , "t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ t₂" -- Right and diagonal (Problem (1, 0))
        -- 3 edges
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ t₂" -- Top, Left, Diagonal
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ t₂" -- Top, Right, Diagonal (Problem (0,1), (1, 0))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ t₂" -- Bottom, Left, Diagonal 
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ t₂" -- Bottom, Right, Diagonal
        , "t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ t₂" -- Left, Right, Diagonal (Problem (1, 0))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ ≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ t₂" -- Top, Bottom, Diagonal (Problem (0, 1))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ ≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬" -- Top, Left, Bottom (Problem (0, 0))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ ≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬" -- Top, Right, Bottom (Problem (0, 0), (0, 1))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭)" -- Top, Left, Right (Problem (0, 0), (1, 0))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭)" -- Bottom, Left, Right (Problem (0, 0))
        -- 4 edges
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ ≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬" -- Top left right bottom (Problem (0,0))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ t₂" -- Top left right diagonal (Problem (1, 0))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ ≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ t₂" -- Top left bottom diagonal
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ ≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ t₂" -- Top right bottom diagonal (Problem (0, 1))
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ t₂" -- Bottom left right diagonal
        -- 5 edges
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ ≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ t₂"
        -- 1 edge and 1 point (Problems with edges are same as before)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟬" -- top edge and (0, 0)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬" -- top edge and (1, 0)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭" -- bottom edge and (0, 1)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟭" -- bottom edge and (1, 1)
        , "t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬" -- left edge and (1, 0)
        , "t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟭" -- left edge and (1, 1)
        , "t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟬" -- right edge and (0, 0)
        , "t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭" -- right edge and (0, 1)
        , "t₁ ≡ t₂ ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭" -- diagonal and (0, 1)
        , "t₁ ≡ t₂ ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬" -- diagonal and (1, 0)
        -- 1 edge and 2 points (Errors are same)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ (t₁ ≡ 𝟬 ∨ t₁ ≡ 𝟭) ∧ t₂ ≡ 𝟬" -- top
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ (t₁ ≡ 𝟬 ∨ t₁ ≡ 𝟭) ∧ t₂ ≡ 𝟭" -- bottom
        , "t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ (t₂ ≡ 𝟬 ∨ t₂ ≡ 𝟭)" -- left
        , "t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟬 ∧ (t₂ ≡ 𝟬 ∨ t₂ ≡ 𝟭)" -- right
        , "t₁ ≡ t₂ ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬" -- diagonal
        -- 2 edge and 1 point
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬" -- top, left, (1, 0)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟬" -- top, right (0, 0)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟭" -- bottom, left, (1, 1)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭" -- bottom, right (0, 1)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ t₂ ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬" -- top, diagonal (1, 0)
        , "≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ t₂ ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭" -- bottom, diagonal (0, 1)
        , "t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ t₂ ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬" -- left, diagonal (1, 0)
        , "t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭) ∨ t₁ ≡ t₂ ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭" -- right, diagonal (0, 1)
        -- 1 triangle 1 point
        , "≤(t₁, t₂) ∨ t₁ ≡ 𝟭 ∧ t₂ ≡ 𝟬" -- top tr
        , "≤(t₂, t₁) ∨ t₁ ≡ 𝟬 ∧ t₂ ≡ 𝟭" -- bottom tr
        -- 1 triangle 1 edge
        , "≤(t₁, t₂) ∨ ≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬" -- top tr, bottom ed
        , "≤(t₁, t₂) ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭)" -- top tr, right ed
        , "≤(t₂, t₁) ∨ ≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭" -- bottom tr, top ed
        , "≤(t₂, t₁) ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭)" -- bottom tr, left ed
        -- 1 triangle 2 edge
        , "≤(t₁, t₂) ∨ ≤(t₁, 𝟭) ∧ t₂ ≡ 𝟬 ∨ t₁ ≡ 𝟭 ∧ ≤(t₂, 𝟭)" -- top triangle
        , "≤(t₂, t₁) ∨ ≤(t₁, 𝟭) ∧ t₂ ≡ 𝟭 ∨ t₁ ≡ 𝟬 ∧ ≤(t₂, 𝟭)" -- bottom triangle
        -- all
        , "⊤"    
    ]