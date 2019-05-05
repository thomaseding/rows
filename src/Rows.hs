{-# LANGUAGE FlexibleContexts #-}

module Rows (
    main,
) where


import AnsiColor (Layer(Foreground, Background), availableColors, colorize)
import Color (Color)
import Color.Names (rainbow)
import Control.Monad ((<=<), filterM)
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Data.Char (isSpace, isPrint)
import Data.Interpolate (interpolate)
import Data.List (group)
import Data.Metric (closest)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Stream as Stream
import Data.Stream (Stream(Cons))
import Rgb (Rgb(Rgb), fromRgb, toRgb)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))


data ColorState = ColorState (Stream Color)


mkColorState :: Stream Color -> ColorState
mkColorState = ColorState


nextColor :: ColorState -> (ColorState, Color)
nextColor (ColorState (c `Cons` cs)) = (ColorState cs, c)


squash :: (Eq a) => [a] -> [a]
squash = take 1 <=< group


fullRainbow :: [Color]
fullRainbow = squash $ concat $ zipWith f rainbow $ tail $ cycle rainbow
    where
        denom = 255
        availableRgbs = map toRgb availableColors
        closest' = closest (Proxy :: Proxy Double) availableRgbs
        f c1 c2 = squash $ map (fromRgb . closest' . toRgb) $ do
            numer <- [0 .. denom]
            return $ interpolate c1 c2 $ numer / denom


data EscapeStatus
    = Escaping
    | NotEscaping


printables :: String -> String
printables = const id $ flip S.evalState NotEscaping . filterM p
    where
        p c = do
            escStatus <- S.get
            case escStatus of
                Escaping -> case c of
                    'm' -> S.put NotEscaping >> return False
                    _ -> return False
                NotEscaping -> case c of
                    '\ESC' -> S.put Escaping >> return False
                    _ -> return $ or $ map ($ c) [isSpace, isPrint]


textWidth :: String -> Int
textWidth = flip S.execState 0 . mapM_ width
    where
        tabWidth = 8
        width c = case c of
            '\t' -> S.gets ((tabWidth -) . (`mod` tabWidth)) >>= \n -> S.modify (+ n)
            _ -> S.modify (+ 1)


colorify :: Options -> String -> L.State ColorState String
colorify opts str = do
    (colorSt, color) <- L.gets nextColor
    L.put colorSt
    return $ colorize layer color $ if layer == Background && padBg
        then str ++ replicate padLen ' '
        else str
    where
        layer = optLayer opts
        padBg = optPadBackgroundColor opts
        cols = optColumns opts
        lineLen = textWidth $ printables str
        padLen = if lineLen == 0
            then cols
            else (cols - (lineLen `mod` cols)) `mod` cols


data Options = Options {
    optLayer :: Layer,
    optPadBackgroundColor :: Bool,
    optColumns :: Int,
    optStartingColors :: Stream Color }


tryRead :: (Read a) => String -> Maybe a
tryRead str = case reads str of
    [(x, "")] -> Just x
    _ -> Nothing


readColumns :: IO Int
readColumns = do
    mColStr <- lookupEnv "COLUMNS"
    return $ case tryRead $ maybe "" id mColStr of
        Just cols -> cols
        Nothing -> 80


parseOptions :: IO Options
parseOptions = do
    args <- getArgs
    layer <- case args of
        [] -> return Foreground
        ["--fg"] -> return Foreground
        ["--bg"] -> return Background
        _ -> exitFailure
    columns <- readColumns
    let grayColors = Stream.cycle $ map fromRgb [Rgb 50 50 50, Rgb 5 5 5]
        rainbowColors = Stream.fromList $ squash $ cycle fullRainbow
        colors = case True of
            True -> rainbowColors
            False -> grayColors
    return Options {
        optLayer = layer,
        optPadBackgroundColor = True,
        optColumns = columns,
        optStartingColors = colors }


processContent :: Options -> String -> String
processContent opts = unlines . flip L.evalState st . mapM f . lines
    where
        st = mkColorState $ optStartingColors opts
        f = colorify opts

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    opts <- parseOptions
    interact $ processContent opts


