module FractalTypes where

import Data.Complex

data FractalType = Mandelbrot | Julia
  deriving (Show, Eq)

data FractalState = FractalState
  { fractalType :: FractalType
  , maxIter :: Int
  , zoom :: Double
  , centerX :: Double
  , centerY :: Double
  , juliaC :: Complex Double
  }

initialState :: FractalState
initialState = FractalState
  { fractalType = Mandelbrot
  , maxIter = 100
  , zoom = 1.0
  , centerX = 0.0
  , centerY = 0.0
  , juliaC = (-0.4) :+ 0.6
  }