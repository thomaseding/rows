{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module AnsiColor (
      ColorCode
    , Layer(..)
    , AnsiColor
    , availableColors
    , colorize
    , uncolorize
    ) where


import AnsiColorChart
import Color
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Metric
import Data.Ord
import Data.Word
import Numeric
import System.Environment


type ColorCode = Word8


data AnsiColor = AnsiColor {
      acColorCode :: ColorCode
    , acLayer :: Layer
    }


colorToAnsi :: Map Color ColorCode
colorToAnsi = Map.fromList $ map (\(x, y) -> (y, x)) chart


availableColors :: [Color]
availableColors = nub $ sortBy (comparing toRgb) $ map snd $ chart


class ToAnsiColorCode a where
    toAnsiColorCode :: a -> ColorCode


instance ToAnsiColorCode Color where
    toAnsiColorCode = toAnsiColorCode . toRgb


instance ToAnsiColorCode Rgb where
    toAnsiColorCode rgb = code
        where
            Just code = Map.lookup color colorToAnsi
            color = fromRgb $ closest (map toRgb availableColors) rgb


instance ToAnsiColorCode ColorCode where
    toAnsiColorCode = id


instance Metric Rgb where
    dist (Rgb r1 g1 b1) (Rgb r2 g2 b2) = dist r1 r2 + dist g1 g2 + dist b1 b2


data Layer = Foreground | Background
    deriving (Show, Eq, Ord)


colorize :: (ToAnsiColorCode a) => Layer -> a -> String -> String
colorize layer color str = "\ESC[" ++ fullCode ++ "m" ++ str ++ "\ESC[0m"
    where
        fullCode = layerCode ++ ";5;" ++ colorCode
        colorCode = show $ toAnsiColorCode color
        layerCode = case layer of
            Foreground -> "38"
            Background -> "48"


reads1 :: (Read a) => String -> Maybe (a, String)
reads1 = listToMaybe . reads


stripColorHeader :: String -> Maybe (AnsiColor, String)
stripColorHeader (stripPrefix "\ESC[38;5;" -> Just str) = Just $ stripColorHeader' Foreground str
stripColorHeader (stripPrefix "\ESC[48;5;" -> Just str) = Just $ stripColorHeader' Background str
stripColorHeader _ = Nothing


stripColorHeader' :: Layer -> String -> (AnsiColor, String)
stripColorHeader' layer str = case reads1 str of
    Just (code, 'm':rest) -> (AnsiColor code layer, rest)


uncolorize :: String -> [(String, Maybe ColorCode)]
uncolorize (stripPrefix "\ESC[38;5;" -> Just str) = undefined
uncolorize (stripPrefix "\ESC[48;5;" -> Just str) = undefined





