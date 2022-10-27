module Project where

import           CodeWorld
import           SquareDrawings
import CubeDrawings (example3)

myPicture :: Picture
myPicture = example3

run :: IO ()
run = drawingOf myPicture
