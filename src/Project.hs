module Project where

import CodeWorld
import TopeLayerData
import SquareDrawings

myPicture :: Picture
myPicture = drawShape (Triangle TriangleLeft)

run :: IO ()
run = drawingOf myPicture