module Fractal.Algorithms where

import Data.Complex
import Fractal.Types (ComplexD)

mandelbrot :: ComplexD -> ComplexD -> Int -> Int
mandelbrot c z iter
    | iter >= maxIterations = 0
    | magnitude z > 2 = iter
    | otherwise = mandelbrot c (z*z + c) (iter + 1)
    where maxIterations = 1000

julia :: ComplexD -> ComplexD -> Int -> Int
julia c z iter
    | iter >= maxIterations = 0
    | magnitude z > 2 = iter
    | otherwise = julia c (z*z + c) (iter + 1)
    where maxIterations = 1000

burningShip :: ComplexD -> ComplexD -> Int -> Int
burningShip c z iter
    | iter >= maxIterations = 0
    | magnitude z > 2 = iter
    | otherwise = burningShip c ((abs(realPart z') :+ abs(imagPart z')) + c) (iter + 1)
    where 
        maxIterations = 1000
        z' = z*z

newton :: ComplexD -> ComplexD -> Int -> Int
newton c z iter
    | iter >= maxIterations = 0
    | magnitude (z' - z) < 0.000001 = iter
    | otherwise = newton c z' (iter + 1)
    where 
        maxIterations = 100
        z' = z - ((z^3 - 1) / (3 * z^2))