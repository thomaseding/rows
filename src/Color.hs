module Color (
      Rgb(..)
    , Color
    , fromRgb
    , toRgb
    , interpolate
    ) where


import Data.Function
import Data.Interpolate
import Data.Word


data Rgb = Rgb Word8 Word8 Word8
    deriving (Eq, Ord, Show)


instance Interpolate Rgb where
    interpolate (Rgb r1 g1 b1) (Rgb r2 g2 b2) k = Rgb r g b
	where
	    interp x y = interpolate x y k
	    r = interp r1 r2
	    g = interp g1 g2
	    b = interp b1 b2


newtype Color = Color Rgb
    deriving (Eq, Ord)


instance Interpolate Color where
    interpolate c1 c2 = fromRgb . (interpolate `on` toRgb) c1 c2


fromRgb :: Rgb -> Color
fromRgb = Color


toRgb :: Color -> Rgb
toRgb (Color rgb) = rgb







