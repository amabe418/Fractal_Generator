module ColorSchemes where

import Graphics.Gloss
import Data.Complex
import Types (ColorScheme(..))

rainbowScheme :: Int -> Int -> Color
rainbowScheme maxIter n
    | n == maxIter = black
    | otherwise = makeColor r g b 1
    where
        t = fromIntegral n / fromIntegral maxIter
        r = 0.5 + 0.5 * cos (2 * pi * t)
        g = 0.5 + 0.5 * cos (2 * pi * t + 2 * pi / 3)
        b = 0.5 + 0.5 * cos (2 * pi * t + 4 * pi / 3)

fireIceScheme :: Int -> Int -> Color
fireIceScheme maxIter n
    | n == maxIter = black
    | otherwise = makeColor r g b 1
    where
        t = fromIntegral n / fromIntegral maxIter
        r = t
        g = t * 0.3
        b = 1 - t

grayscaleScheme :: Int -> Int -> Color
grayscaleScheme maxIter n
    | n == maxIter = black
    | otherwise = makeColor v v v 1
    where
        v = fromIntegral n / fromIntegral maxIter

oceanScheme :: Int -> Int -> Color
oceanScheme maxIter n
    | n == maxIter = black
    | otherwise = makeColor r g b 1
    where
        t = fromIntegral n / fromIntegral maxIter
        r = 0
        g = 0.5 * t
        b = t

getColorFunc :: ColorScheme -> (Int -> Int -> Color)
getColorFunc Rainbow = rainbowScheme
getColorFunc FireIce = fireIceScheme
getColorFunc Grayscale = grayscaleScheme
getColorFunc Ocean = oceanScheme