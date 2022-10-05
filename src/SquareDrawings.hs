{-# LANGUAGE OverloadedStrings #-}
module SquareDrawings where

import CodeWorld
import TopeLayerData

baseLayer :: Picture
baseLayer = colored grey $ thickRectangle 0.1 4 4

createThickPolygon :: [Point] -> Picture
createThickPolygon = thickPolygon 0.1

createPoint :: Picture
createPoint = solidCircle 0.15

mergeShapes :: [SquareTopeShape] -> Picture
mergeShapes = foldr ((<>) . drawShape') blank

drawShape' :: SquareTopeShape -> Picture
drawShape' (Point PointTopRight) = translated 2 2 createPoint
drawShape' (Point PointTopLeft) = translated (-2) 2 createPoint
drawShape' (Point PointBottomRight) = translated 2 (-2) createPoint
drawShape' (Point PointBottomLeft) = translated (-2) (-2) createPoint

drawShape' (Edge EdgeTop) = createThickPolygon [(-2, 2), (2, 2)] 
    <> mergeShapes [Point PointTopLeft, Point PointTopRight]
drawShape' (Edge EdgeBottom) = createThickPolygon [(-2, -2), (2, -2)]
    <> mergeShapes [Point PointBottomLeft, Point PointBottomRight]
drawShape' (Edge EdgeLeft) = createThickPolygon [(-2, 2), (-2, -2)]
    <> mergeShapes [Point PointTopLeft, Point PointBottomLeft]
drawShape' (Edge EdgeRight) = createThickPolygon [(2, 2), (2, -2)]
    <> mergeShapes [Point PointTopRight, Point PointBottomRight]
drawShape' (Edge EdgeDiag) = createThickPolygon [(-2, 2), (2, -2)]
    <> mergeShapes [Point PointTopLeft, Point PointBottomRight]

drawShape' (Triangle TriangleLeft) = mergeShapes [
        Edge EdgeLeft, Edge EdgeBottom, Edge EdgeDiag
    ]
drawShape' (Triangle TriangleRight) = mergeShapes [
        Edge EdgeRight, Edge EdgeTop, Edge EdgeDiag
    ]


drawShape :: SquareTopeShape -> Picture
drawShape shape = colored red (drawShape' shape) <> baseLayer