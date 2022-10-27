module Project where

import           CodeWorld
import           CubeDrawings   (example3)
import           SquareDrawings

myPicture :: Picture
myPicture = example3

run :: IO ()
run = drawingOf myPicture
