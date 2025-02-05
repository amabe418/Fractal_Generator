module Fractal.Generator where

import Data.Complex
import Fractal.Types
import Fractal.Algorithms

generateFractal :: FractalParams -> [[Int]]
generateFractal params = 
    [[calculatePoint x y | x <- [0..w-1]] | y <- [0..h-1]]
    where
        w = width params
        h = height params
        dx = (xMax params - xMin params) / fromIntegral w
        dy = (yMax params - yMin params) / fromIntegral h
        
        calculatePoint x y =
            let real = xMin params + dx * fromIntegral x
                imag = yMin params + dy * fromIntegral y
                point = real :+ imag
            in case fractalType params of
                "mandelbrot" -> mandelbrot point (0 :+ 0) 0
                "julia" -> julia ((-0.4) :+ (0.6)) point 0
                "burningship" -> burningShip point (0 :+ 0) 0
                "newton" -> newton point point 0
                _ -> 0