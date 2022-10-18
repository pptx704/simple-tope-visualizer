module TopeLayerData where

import CodeWorld
import qualified RSTT.Syntax.Abs as RSTT
import           Data.String     (IsString (..))
import qualified RSTT.Syntax.Par as RSTT

data BasicShape = BasicShape
  { basicShapeTope   :: RSTT.Tope
  , basicShapePoints :: [CodeWorld.Point]
  }

instance IsString RSTT.Tope where
  fromString = unsafeFromRight . RSTT.pTope . RSTT.myLexer
    where
      unsafeFromRight (Right x)  = x
      unsafeFromRight (Left msg) = error msg