module Project where

import           CodeWorld
import           SquareDrawings

myPicture :: Picture
myPicture = example1

run :: IO ()
run = drawingOf myPicture
