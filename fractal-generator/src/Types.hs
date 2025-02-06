module Types where

import Data.Complex

-- Enhanced FractalType with animation support
data FractalType = 
    Mandelbrot
    | Julia
    | Tree
    | Sierpinski
    | KochSnowflake
    | BurningShip
    deriving (Eq, Show)

data ColorScheme = 
    Rainbow
    | FireIce
    | Grayscale
    | Ocean
    | Psychedelic  -- New animated color scheme
    deriving (Eq, Show)

-- Enhanced state with animation parameters
data FractalState = FractalState
    { fractalType :: FractalType
    , colorScheme :: ColorScheme
    , zoom :: Float
    , center :: (Float, Float)
    , maxIterations :: Int
    , juliaC :: Complex Float
    , treeDepth :: Int
    , renderQuality :: Int -- 1 = highest, 4 = lowest
    , showHelp :: Bool
    , animationTime :: Float      -- New: For animation timing
    , animationSpeed :: Float     -- New: Control animation speed
    , mousePos :: (Float, Float)  -- New: For interactive effects
    , isPaused :: Bool           -- New: Animation pause state
    , bufferData :: Maybe BufferData  -- New: For double buffering
    }

-- New: Structure for double buffering
data BufferData = BufferData
    { bufferPoints :: [(Float, Float, Int)]  -- Cached points and iterations
    , bufferZoom :: Float                    -- Zoom level when buffer was created
    , bufferCenter :: (Float, Float)         -- Center when buffer was created
    }