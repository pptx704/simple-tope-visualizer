{-# LANGUAGE OverloadedStrings #-}

module Project where

import           CodeWorld
import           CubeDrawings
import qualified RSTT.Syntax.Abs as RSTT
import           SquareDrawings
import           Visualize       (visualize)

myPicture :: Picture
myPicture = example1

run :: IO ()
--run = drawingOf myPicture
run = visualize "⊥"
