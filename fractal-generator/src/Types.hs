module Types where

import Data.Complex

data FractalType = Mandelbrot 
                | Julia 
                | Tree 
                | Sierpinski 
                | KochSnowflake
                | BurningShip
    deriving (Eq, Show)

data ColorScheme = Rainbow 
                | FireIce 
                | Grayscale 
                | Ocean
    deriving (Eq, Show)

data FractalState = FractalState
    { fractalType :: FractalType
    , colorScheme :: ColorScheme
    , zoom :: Float
    , center :: (Float, Float)
    , maxIterations :: Int
    , juliaC :: Complex Float
    , treeDepth :: Int
    , renderQuality :: Int  -- 1 = highest, 4 = lowest
    , showHelp :: Bool
    }