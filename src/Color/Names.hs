module Color.Names (
    white,
    black,
    red,
    orange,
    yellow,
    green,
    blue,
    indigo,
    violet,
    rainbow,
) where


import Color (Color)
import Rgb (Rgb(Rgb), fromRgb)


white :: Color
white = fromRgb $ Rgb 255 255 255


black :: Color
black = fromRgb $ Rgb 0 0 0


red :: Color
red = fromRgb $ Rgb 255 0 0


orange :: Color
orange = fromRgb $ Rgb 255 165 0


yellow :: Color
yellow = fromRgb $ Rgb 255 255 0


green :: Color
green = fromRgb $ Rgb 0 255 0


blue :: Color
blue = fromRgb $ Rgb 0 0 255


indigo :: Color
indigo = fromRgb $ Rgb 75 0 130


violet :: Color
violet = fromRgb $ Rgb 238 130 238


rainbow :: [Color]
rainbow = [red, orange, yellow, green, blue, indigo, violet]


