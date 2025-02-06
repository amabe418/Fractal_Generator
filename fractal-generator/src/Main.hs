module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Codec.Picture
import Types
import Fractals
import ColorSchemes
import Data.Complex
import Data.Time
import System.Directory
import Control.Monad (when)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V
import Control.Parallel.Strategies
import Fractals (generateFractal, pixelToColor)

initialState :: FractalState
initialState = FractalState 
    { fractalType = Mandelbrot
    , colorScheme = Rainbow
    , zoom = 200
    , center = (0, 0)
    , maxIterations = 100
    , juliaC = (-0.4) :+ 0.6
    , treeDepth = 10
    , renderQuality = 2
    , showHelp = False
    }

main :: IO ()
main = do
    createDirectoryIfMissing True "fractals"
    play window
         backgroundColor
         30
         initialState
         drawWorld
         handleEvent
         updateFractal

window :: Display
window = InWindow "Enhanced Fractal Generator" (800, 600) (100, 100)

backgroundColor :: Color
backgroundColor = black

drawWorld :: FractalState -> Picture
drawWorld state
    | showHelp state = Pictures [generateFractal state, drawHelp]
    | otherwise = generateFractal state

drawHelp :: Picture
drawHelp = Translate (-380) 250 $ Scale 0.1 0.1 $ Text $
    "Controls:\n" ++
    "M: Mandelbrot  J: Julia  T: Tree\n" ++
    "S: Sierpinski  K: Koch  B: BurningShip\n" ++
    "1-4: Color Schemes\n" ++
    "Q/E: Quality Down/Up\n" ++
    "+/-: Zoom\n" ++
    "Arrows: Move\n" ++
    "Space: Save Image\n" ++
    "H: Toggle Help"

-- Convert Gloss Color to JuicyPixels PixelRGB8
colorToPixelRGB8 :: Color -> PixelRGB8
colorToPixelRGB8 color = 
    let (r, g, b, _) = rgbaOfColor color
    in PixelRGB8 (truncate $ r * 255) (truncate $ g * 255) (truncate $ b * 255)

-- Save the current view as a PNG file
saveAsPNG :: FractalState -> IO ()
saveAsPNG state = do
    time <- getCurrentTime
    let filename = "fractals/fractal_" ++ 
                  show (fractalType state) ++ "_" ++ 
                  formatTime defaultTimeLocale "%Y%m%d_%H%M%S" time ++ 
                  ".png"
    let width = 800
    let height = 600
    let image = generateImage (\x y -> 
            colorToPixelRGB8 $ 
            pixelToColor state 
                (fromIntegral x - fromIntegral width / 2, 
                 fromIntegral y - fromIntegral height / 2))
            width height
    savePngImage filename (ImageRGB8 image)
    putStrLn $ "Saved fractal as " ++ filename

handleEvent :: Event -> FractalState -> FractalState
handleEvent (EventKey (Char 'm') Down _ _) state = 
    state { fractalType = Mandelbrot }
handleEvent (EventKey (Char 'j') Down _ _) state = 
    state { fractalType = Julia }
handleEvent (EventKey (Char 't') Down _ _) state = 
    state { fractalType = Tree }
handleEvent (EventKey (Char 's') Down _ _) state = 
    state { fractalType = Sierpinski }
handleEvent (EventKey (Char 'k') Down _ _) state = 
    state { fractalType = KochSnowflake }
handleEvent (EventKey (Char 'b') Down _ _) state = 
    state { fractalType = BurningShip }
handleEvent (EventKey (Char '1') Down _ _) state = 
    state { colorScheme = Rainbow }
handleEvent (EventKey (Char '2') Down _ _) state = 
    state { colorScheme = FireIce }
handleEvent (EventKey (Char '3') Down _ _) state = 
    state { colorScheme = Grayscale }
handleEvent (EventKey (Char '4') Down _ _) state = 
    state { colorScheme = Ocean }
handleEvent (EventKey (Char 'q') Down _ _) state = 
    state { renderQuality = min 4 (renderQuality state + 1) }
handleEvent (EventKey (Char 'e') Down _ _) state = 
    state { renderQuality = max 1 (renderQuality state - 1) }
handleEvent (EventKey (Char 'h') Down _ _) state = 
    state { showHelp = not (showHelp state) }
handleEvent (EventKey (Char ' ') Down _ _) state = 
    unsafePerformIO $ do
        saveAsPNG state
        return state
handleEvent (EventKey (Char '+') Down _ _) state = 
    state { zoom = zoom state * 1.1 }
handleEvent (EventKey (Char '-') Down _ _) state = 
    state { zoom = zoom state / 1.1 }
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state = 
    state { center = (fst (center state), snd (center state) + 10/zoom state) }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state = 
    state { center = (fst (center state), snd (center state) - 10/zoom state) }
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state = 
    state { center = (fst (center state) - 10/zoom state, snd (center state)) }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = 
    state { center = (fst (center state) + 10/zoom state, snd (center state)) }
handleEvent _ state = state

updateFractal :: Float -> FractalState -> FractalState
updateFractal _ state = state