{-# LANGUAGE OverloadedStrings #-}

module Project where

import           CodeWorld
import           CubeDrawings
import           SquareDrawings
import           Visualize      (visualize)

myPicture :: Picture
myPicture = example1

run :: IO ()
run = visualize "u ≡ 𝟬 ∧ ≤(s, t) ∨ s ≡ 𝟭 ∧ t ≡ u"
