module ColorSchemes where

import Graphics.Gloss
import Data.Complex
import Types (ColorScheme(..))

-- Optimized color calculation using pre-calculated values
rainbowScheme :: Int -> Int -> Float -> Color  -- Added time parameter for animation
rainbowScheme maxIter n time
    | n == maxIter = black
    | otherwise = makeColor r g b 1
    where
        t = fromIntegral n / fromIntegral maxIter
        phase = t * 2 * pi + time  -- Animate colors over time
        r = 0.5 + 0.5 * cos phase
        g = 0.5 + 0.5 * cos (phase + 2 * pi / 3)
        b = 0.5 + 0.5 * cos (phase + 4 * pi / 3)

fireIceScheme :: Int -> Int -> Float -> Color
fireIceScheme maxIter n time
    | n == maxIter = black
    | otherwise = makeColor r g b 1
    where
        t = fromIntegral n / fromIntegral maxIter
        wave = sin (time * 2) * 0.2  -- Add wave effect
        r = t + wave
        g = t * 0.3
        b = 1 - t + wave

grayscaleScheme :: Int -> Int -> Float -> Color
grayscaleScheme maxIter n time
    | n == maxIter = black
    | otherwise = makeColor v v v 1
    where
        t = fromIntegral n / fromIntegral maxIter
        v = (t + sin (time * 2) * 0.1) `min` 1.0  -- Subtle pulsing effect

oceanScheme :: Int -> Int -> Float -> Color
oceanScheme maxIter n time
    | n == maxIter = black
    | otherwise = makeColor r g b 1
    where
        t = fromIntegral n / fromIntegral maxIter
        wave = sin (time * 3 + t * pi) * 0.2  -- Water-like wave effect
        r = 0
        g = (0.5 * t + wave) `min` 1.0
        b = (t + wave) `min` 1.0

-- New: Psychedelic color scheme with strong animation
psychedelicScheme :: Int -> Int -> Float -> Color
psychedelicScheme maxIter n time
    | n == maxIter = black
    | otherwise = makeColor r g b 1
    where
        t = fromIntegral n / fromIntegral maxIter
        r = 0.5 + 0.5 * sin (t * pi * 2 + time * 3)
        g = 0.5 + 0.5 * sin (t * pi * 2 + time * 2)
        b = 0.5 + 0.5 * sin (t * pi * 2 + time)

getColorFunc :: ColorScheme -> (Int -> Int -> Float -> Color)
getColorFunc Rainbow = rainbowScheme
getColorFunc FireIce = fireIceScheme
getColorFunc Grayscale = grayscaleScheme
getColorFunc Ocean = oceanScheme
getColorFunc Psychedelic = psychedelicScheme