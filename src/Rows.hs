{-# LANGUAGE FlexibleContexts #-}

module Rows (
    main,
) where


import AnsiColor
import Color
import Colors
import Control.Monad
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Data.Char
import Data.List
import Data.Metric
import Data.Proxy
import qualified Data.Stream as Stream
import Data.Stream (Stream(Cons))
import Data.Word
import System.Environment
import System.Exit
import System.IO


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
        st = mkColorState {- [mkGray 50, mkGray 5] -} $ squash $ cycle fullRainbow


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


modUp :: Integral a => a -> a -> a
x `modUp` m = case x `mod` m of
    0 -> m
    y -> y


textWidth :: String -> Int
textWidth = flip S.execState 0 . mapM_ width
    where
        tabWidth = 8
        width c = case c of
            '\t' -> S.gets ((tabWidth -) . (`mod` tabWidth)) >>= \n -> S.modify (+ n)
            _ -> S.modify (+ 1)


