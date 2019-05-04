{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module AnsiColor (
    Layer(..),
    AnsiColor,
    availableColors,
    colorize,
    uncolorize,
) where


import AnsiColorChart (ColorCode, chart)
import Color (Color, Rgb, fromRgb, toRgb)
import qualified Data.DList as DList
import Data.DList (DList)
import Data.List (nub, sortBy, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Metric (closest)
import Data.Ord (comparing)
import Data.Proxy (Proxy(Proxy))


data AnsiColor = AnsiColor {
    acColorCode :: ColorCode,
    acLayer :: Layer }
    deriving (Show, Eq)


colorToAnsi :: Map Color ColorCode
colorToAnsi = Map.fromList $ map (\(x, y) -> (y, x)) chart


availableColors :: [Color]
availableColors = nub $ sortBy (comparing toRgb) $ map snd $ chart


class ToAnsiColorCode a where
    toAnsiColorCode :: a -> ColorCode


instance ToAnsiColorCode Color where
    toAnsiColorCode = toAnsiColorCode . toRgb


instance ToAnsiColorCode Rgb where
    toAnsiColorCode rgb = case Map.lookup color colorToAnsi of
        Just colorCode -> colorCode
        Nothing -> error "toAnsiColorCode @Rgb: Internal logic error."
        where
            rgbs = map toRgb availableColors
            color = fromRgb $ closest (Proxy :: Proxy Double) rgbs rgb


instance ToAnsiColorCode ColorCode where
    toAnsiColorCode = id


data Layer = Foreground | Background
    deriving (Show, Eq, Ord)


colorize :: (ToAnsiColorCode a) => Layer -> a -> String -> String
colorize layer color str = let
    colorCode = show $ toAnsiColorCode color
    layerCode = case layer of
        Foreground -> "38"
        Background -> "48"
    in "\ESC[" ++ layerCode ++ ";5;" ++ colorCode ++ "m" ++ str ++ "\ESC[0m"


stripColorHeader :: String -> Maybe (AnsiColor, String)
stripColorHeader = \case
    (stripPrefix "\ESC[38;5;" -> Just str) -> Just $ stripColorHeader' Foreground str
    (stripPrefix "\ESC[48;5;" -> Just str) -> Just $ stripColorHeader' Background str
    _ -> Nothing


stripColorHeader' :: Layer -> String -> (AnsiColor, String)
stripColorHeader' layer str = case reads str of
    (code, 'm':rest) : _ -> (AnsiColor code layer, rest)
    _ -> error "stripColorHeader': Invalid ColorCode escape sequence."


uncolorize :: String -> [(String, Maybe AnsiColor)]
uncolorize = filter p . map f . merge . uncolorize' Nothing
    where
        f (s, mc) = (DList.toList s, mc)
        p (s, _) = not $ null s


merge :: [(DList Char, Maybe AnsiColor)] -> [(DList Char, Maybe AnsiColor)]
merge = \case
    p1@(s1, mc1) : p2@(s2, mc2) : rest -> case mc1 == mc2 of
        True -> let
            s' = s1 `DList.append` s2
            in merge $ (s', mc1) : rest
        False -> p1 : merge (p2 : rest)
    [p] -> [p]
    [] -> []


uncolorize' :: Maybe AnsiColor -> String -> [(DList Char, Maybe AnsiColor)]
uncolorize' mColor = \case
    (stripColorHeader -> Just (color, rest)) -> let
        jColor = Just color
        in (DList.empty, jColor) : uncolorize' jColor rest
    (stripPrefix "\ESC[0m" -> Just rest) -> let
        in (DList.empty, Nothing) : uncolorize' Nothing rest
    c : rest -> (DList.singleton c, mColor) : uncolorize' mColor rest
    "" -> []


