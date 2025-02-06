module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
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
import Graphics.Gloss.Raster.Field (makePicture)


import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Animate (animateFixedIO)
import Foreign.Marshal.Array (withArray, peekArray) -- ✅ `peekArray` comes from here
import Foreign.Storable -- ✅ Keep this, but without `peekArray`import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CUChar)
import Data.Word (Word8)
import Control.Monad (void)


-- Enhanced initial state with animation parameters
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
    , animationTime = 0
    , animationSpeed = 1.0
    , mousePos = (0, 0)
    , isPaused = False
    , bufferData = Nothing
    }

main :: IO ()
main = do
    createDirectoryIfMissing True "fractals"
    play window
         backgroundColor
         60  -- Increased FPS for smoother animation
         initialState
         drawWorld
         handleEvent
         updateWorld

window :: Display
window = InWindow "Animated Fractal Generator" (800, 600) (100, 100)

backgroundColor :: Color
backgroundColor = black

-- Enhanced drawing with animation
drawWorld :: FractalState -> Picture
drawWorld state = Pictures
    [ generateFractal state
    , if showHelp state then drawHelp else Blank
    , drawUI state
    ]

-- Enhanced UI with animation controls
drawUI :: FractalState -> Picture
drawUI state = Pictures
    [ Translate (-390) 280 $ Scale 0.1 0.1 $ Text $
        "FPS: " ++ show (floor (1/animationSpeed state))
    , Translate (-390) 260 $ Scale 0.1 0.1 $ Text $
        if isPaused state then "PAUSED" else "PLAYING"
    , Translate (-390) 240 $ Scale 0.1 0.1 $ Text $
        "Speed: " ++ show (animationSpeed state)
    ]

drawHelp :: Picture
drawHelp = Translate (-380) 250 $ Scale 0.1 0.1 $ Text $
    "Controls:\n" ++
    "M: Mandelbrot  J: Julia  T: Tree\n" ++
    "S: Sierpinski  K: Koch  B: BurningShip\n" ++
    "1-5: Color Schemes\n" ++
    "Q/E: Quality Down/Up\n" ++
    "+/-: Zoom\n" ++
    "Arrows: Move\n" ++
    "Space: Save Image\n" ++
    "P: Pause Animation\n" ++
    "R: Reset Animation\n" ++
    "[/]: Adjust Speed\n" ++
    "H: Toggle Help"

-- Continuing from where you left off...
handleEvent :: Event -> FractalState -> FractalState
handleEvent (EventKey (Char 'p') Down _ _) state = 
    state { isPaused = not (isPaused state) }
handleEvent (EventKey (Char 'r') Down _ _) state =
    state { animationTime = 0 }
handleEvent (EventKey (Char '[') Down _ _) state =
    state { animationSpeed = max 0.1 (animationSpeed state - 0.1) }
handleEvent (EventKey (Char ']') Down _ _) state =
    state { animationSpeed = min 2.0 (animationSpeed state + 0.1) }
handleEvent (EventKey (Char 'm') Down _ _) state =
    state { fractalType = Mandelbrot, center = (0, 0), zoom = 200 }
handleEvent (EventKey (Char 'j') Down _ _) state =
    state { fractalType = Julia, center = (0, 0), zoom = 200 }
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
handleEvent (EventKey (Char '5') Down _ _) state =
    state { colorScheme = Psychedelic }
handleEvent (EventKey (Char 'q') Down _ _) state =
    state { renderQuality = min 4 (renderQuality state + 1) }
handleEvent (EventKey (Char 'e') Down _ _) state =
    state { renderQuality = max 1 (renderQuality state - 1) }
handleEvent (EventKey (Char 'h') Down _ _) state =
    state { showHelp = not (showHelp state) }
handleEvent (EventKey (Char '+') Down _ _) state =
    state { zoom = zoom state * 1.2 }
handleEvent (EventKey (Char '-') Down _ _) state =
    state { zoom = zoom state / 1.2 }
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state =
    state { center = (fst (center state), snd (center state) + 10 / zoom state) }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state =
    state { center = (fst (center state), snd (center state) - 10 / zoom state) }
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state =
    state { center = (fst (center state) - 10 / zoom state, snd (center state)) }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state =
    state { center = (fst (center state) + 10 / zoom state, snd (center state)) }
-- handleEvent (EventKey (Char ' ') Down _ _) state = unsafePerformIO $ do
--     let filename = "fractals/fractal_" ++ show (fractalType state) ++ "_" ++ 
--                   show (floor (animationTime state)) ++ ".png"
--     savePictureToPNG 800 600 filename (generateFractal state)
--     return state
handleEvent (EventMotion pos) state =
    state { mousePos = pos }
handleEvent _ state = state

-- Update world state for animation
updateWorld :: Float -> FractalState -> FractalState
updateWorld dt state
    | isPaused state = state
    | otherwise = state
        { animationTime = animationTime state + dt * animationSpeed state
        , juliaC = (-0.4 + 0.1 * sin (animationTime state)) :+ 
                  (0.6 + 0.1 * cos (animationTime state))
        }

-- -- Convert Gloss Picture to JuicyPixels Image
-- pictureToImage :: Int -> Int -> Picture -> IO (Image PixelRGBA8)
-- pictureToImage width height picture = do
--     renderer <- initState
--     let viewport = (0, 0, fromIntegral width, fromIntegral height)
--     pixels <- withArray (replicate (width * height * 4) 0) $ \ptr -> do
--         void $ renderToBuffer renderer viewport white picture
--         peekArray (width * height * 4) ptr
--     return $ generateImage (pixelRenderer pixels) width height
--   where
--     pixelRenderer pixels x y =
--         let idx = (y * width + x) * 4
--             r = pixels !! (idx + 0)
--             g = pixels !! (idx + 1)
--             b = pixels !! (idx + 2)
--             a = pixels !! (idx + 3)
--         in PixelRGBA8 r g b a

-- -- Save Picture as PNG
-- savePictureToPNG :: Int -> Int -> FilePath -> Picture -> IO ()
-- savePictureToPNG width height filepath picture = do
--     image <- pictureToImage width height picture
--     savePngImage filepath (ImageRGBA8 image)