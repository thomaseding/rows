module Color (
    Rgb(..),
    Color,
    fromRgb,
    toRgb,
) where


import Data.Function (on)
import Data.Interpolate (Interpolate(interpolate))
import Data.Metric (Metric(dist))
import Data.Word (Word8)


data Rgb = Rgb Word8 Word8 Word8
    deriving (Eq, Ord, Show)


instance Interpolate Rgb where
    interpolate (Rgb r1 g1 b1) (Rgb r2 g2 b2) k = Rgb r g b
        where
            interp x y = interpolate x y k
            r = interp r1 r2
            g = interp g1 g2
            b = interp b1 b2


instance Metric Rgb where
    dist (Rgb r1 g1 b1) (Rgb r2 g2 b2) = dist r1 r2 + dist g1 g2 + dist b1 b2


newtype Color = Color Rgb
    deriving (Eq, Ord)


instance Interpolate Color where
    interpolate c1 c2 = fromRgb . (interpolate `on` toRgb) c1 c2


fromRgb :: Rgb -> Color
fromRgb = Color


toRgb :: Color -> Rgb
toRgb (Color rgb) = rgb


