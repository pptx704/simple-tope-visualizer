module TopeLayerData where

data SquareTopeShape = Edge SquareTopeEdge | Triangle SquareTopeTriangle | Point SquareTopePoint
data SquareTopeEdge = EdgeTop | EdgeBottom | EdgeLeft | EdgeRight | EdgeDiag
data SquareTopeTriangle = TriangleRight | TriangleLeft
data SquareTopePoint = PointTopLeft | PointTopRight | PointBottomLeft | PointBottomRight