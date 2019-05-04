{-# LANGUAGE FlexibleContexts #-}

module Rows (
    main,
) where


import AnsiColor (Layer(Foreground, Background), availableColors, colorize)
import Color (Color, Rgb(Rgb), fromRgb, toRgb)
import Colors (rainbow)
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
import Data.Word (Word8)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))


mkGray :: Word8 -> Color
mkGray n = fromRgb $ Rgb n n n


data Options = Options {
    optLayer :: Layer,
    optPadBackgroundColor :: Bool,
    optColumns :: Int }


main :: IO ()
main = do
    args <- getArgs
    layer <- case args of
        [] -> return Foreground
        ["--fg"] -> return Foreground
        ["--bg"] -> return Background
        _ -> exitFailure
    hSetBuffering stdout LineBuffering
    columns <- readColumns
    let opts = Options {
        optLayer = layer,
        optPadBackgroundColor = True,
        optColumns = columns }
    interact $ unlines . flip L.evalState st . mapM (colorify opts) . lines
    where
        useRainbow = True
        rainbowColors = squash $ cycle fullRainbow
        grayColors = [mkGray 50, mkGray 5]
        st = mkColorState $ case useRainbow of
            True -> rainbowColors
            False -> grayColors


readColumns :: IO Int
readColumns = do
    colStr <- getEnv "COLUMNS"
    return $ case tryRead colStr of
        Just cols -> cols
        Nothing -> 80


tryRead :: (Read a) => String -> Maybe a
tryRead str = case reads str of
    [(x, "")] -> Just x
    _ -> Nothing


fullRainbow :: [Color]
fullRainbow = squash $ concat $ zipWith f rainbow $ tail $ cycle rainbow
    where
        denom = 255
        availableRgbs = map toRgb availableColors
        f c1 c2 = squash $ map (fromRgb . closest (Proxy :: Proxy Double) availableRgbs . toRgb)  $ do
            numer <- [0 .. denom]
            return $ interpolate c1 c2 $ numer / denom


squash :: (Eq a) => [a] -> [a]
squash = take 1 <=< group


data ColorState = ColorState (Stream Color)


mkColorState :: [Color] -> ColorState
mkColorState = ColorState . Stream.cycle


nextColor :: ColorState -> (ColorState, Color)
nextColor (ColorState (c `Cons` cs)) = (ColorState cs, c)


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


data EscapeStatus = Escaping | NotEscaping


printables :: String -> String
printables = const id $ flip S.evalState NotEscaping . filterM predicate
    where
        predicate c = do
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


