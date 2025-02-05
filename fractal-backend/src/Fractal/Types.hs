{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Fractal.Types where

import GHC.Generics
import Data.Complex
import Data.Aeson

type ComplexD = Complex Double

data FractalParams = FractalParams 
    { width :: Int
    , height :: Int
    , maxIter :: Int
    , xMin :: Double
    , xMax :: Double
    , yMin :: Double
    , yMax :: Double
    , fractalType :: String
    , colorPalette :: String
    } deriving (Show, Generic)

instance FromJSON FractalParams
instance ToJSON FractalParams