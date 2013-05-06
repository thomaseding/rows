module AnsiColor (
      Layer(..)
    , availableColors
    , colorize
    , main
    ) where


import AnsiColorChart
import Color
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Metric
import Data.Ord
import Data.Word
import Numeric
import System.Environment


type ColorCode = Word8


colorToAnsi :: Map Color ColorCode
colorToAnsi = Map.fromList $ map (\(x, y) -> (y, x)) chart


availableColors :: [Color]
availableColors = nub $ sortBy (comparing toRgb) $ map snd $ chart


class ToAnsiColorCode a where
    toAnsiColorCode :: a -> String


instance ToAnsiColorCode Color where
    toAnsiColorCode = toAnsiColorCode . toRgb


instance ToAnsiColorCode Rgb where
    toAnsiColorCode rgb = toAnsiColorCode index
	where
	    Just index = Map.lookup color colorToAnsi
	    color = fromRgb $ closest (map toRgb availableColors) rgb


instance Metric Rgb where
    dist (Rgb r1 g1 b1) (Rgb r2 g2 b2) = dist r1 r2 + dist g1 g2 + dist b1 b2


instance ToAnsiColorCode Word8 where
    toAnsiColorCode = show


data Layer = Foreground | Background
    deriving (Show, Eq, Ord)


colorize :: (ToAnsiColorCode a) => Layer -> a -> String -> String
colorize layer color str = "\ESC[" ++ fullCode ++ "m" ++ str ++ "\ESC[0m"
    where
	fullCode = layerCode ++ ";5;" ++ colorCode
	colorCode = toAnsiColorCode color
	layerCode = case layer of
	    Foreground -> "38"
	    Background -> "48"


tryRead :: (Read a) => String -> Maybe a
tryRead str = case reads str of
    [(x, "")] -> Just x
    _ -> Nothing


tryReadWord8 :: String -> Maybe Word8
tryReadWord8 str = case tryRead str of
    Nothing -> Nothing
    Just n -> if 0 <= n && n < 256
	then Just $ fromInteger n
	else Nothing


main :: IO ()
main = do
    [arg0, arg1, arg3] <- getArgs
    layer <- case arg0 of
	"fg" -> return Foreground
	"bg" -> return Background
    case tryReadWord8 arg1 of
	Just code256 -> putStrLn $ colorize layer code256 arg3


uncolorize :: String -> [(String, Maybe Word8)]
uncolorize = undefined





