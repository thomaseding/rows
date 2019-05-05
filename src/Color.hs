module Color (
    Color,
) where


import Data.Function (on)
import Data.Interpolate (Interpolate(interpolate))
import Rgb (Rgb, RgbLike(fromRgb, toRgb))


newtype Color = RgbColor Rgb
    deriving (Eq, Ord)


instance Interpolate Color where
    interpolate c1 c2 = fromRgb . (interpolate `on` toRgb) c1 c2


instance RgbLike Color where
    fromRgb = RgbColor
    toRgb (RgbColor rgb) = rgb


