module TopeLayerData where

import           Data.String     (IsString (..))
import qualified RSTT.Syntax.Abs as RSTT
import qualified RSTT.Syntax.Par as RSTT

data BasicShape a = BasicShape
  { basicShapeTope   :: RSTT.Tope
  , basicShapePoints :: [a]
  }

data Layer = None | Front | Middle | Back
  deriving Eq

instance IsString RSTT.Tope where
  fromString = unsafeFromRight . RSTT.pTope . RSTT.myLexer
    where
      unsafeFromRight (Right x)  = x
      unsafeFromRight (Left msg) = error msg
