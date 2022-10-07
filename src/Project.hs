module Project where

import           CodeWorld
import           SquareDrawings
import           TopeLayerData

myPicture :: Picture
myPicture = example1

run :: IO ()
run = drawingOf myPicture
