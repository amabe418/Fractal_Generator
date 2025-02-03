module FractalLogic where

import Data.Complex
import FractalTypes

-- Core fractal computation functions
mandelbrotIter :: Int -> Complex Double -> Int
mandelbrotIter maxN c = length $ takeWhile (\z -> magnitude z <= 2) $ take maxN $ iterate (\z -> z*z + c) 0

juliaIter :: Int -> Complex Double -> Complex Double -> Int
juliaIter maxN c z0 = length $ takeWhile (\z -> magnitude z <= 2) $ take maxN $ iterate (\z -> z*z + c) z0

-- Color calculation
iterToColor :: Int -> Int -> [Double]
iterToColor i maxN
  | i == maxN = [0, 0, 0]  -- Black for points in set
  | otherwise = 
      let t = fromIntegral i / fromIntegral maxN
      in [t, t*t, sqrt t]  -- Smooth coloring

-- Coordinate transformation
pixelToComplex :: (Int, Int) -> (Int, Int) -> Double -> Double -> Double -> Complex Double
pixelToComplex (width, height) (x, y) zoom centerX centerY = 
  let rx = (fromIntegral x - fromIntegral width/2) / (fromIntegral width/4) / zoom + centerX
      ry = (fromIntegral y - fromIntegral height/2) / (fromIntegral height/4) / zoom + centerY
  in rx :+ ry

-- Calculate fractal value for a point
getFractalValue :: FractalState -> Complex Double -> Int
getFractalValue state z = case fractalType state of
  Mandelbrot -> mandelbrotIter (maxIter state) z
  Julia -> juliaIter (maxIter state) (juliaC state) z