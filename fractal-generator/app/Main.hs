{-# LANGUAGE BangPatterns #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Bits ((.&.))


-- Tamaño de la ventana
width, height :: Int
width = 800
height = 600

-- Límite de iteraciones
maxIter :: Int
maxIter = 200

-- Tipos de fractales
data FractalType = Mandelbrot | Julia | BurningShip | Tricorn | Newton | Phoenix | Sierpinski deriving (Eq, Show)

-- Definir el estado de la aplicación
data World = World { zoomFactor :: Float
                   , offsetX :: Float
                   , offsetY :: Float
                   , fractalType :: FractalType
                   }

-- Función que determina si un punto pertenece al conjunto de Mandelbrot
mandelbrot :: Float -> Float -> Int
mandelbrot x y = length $ takeWhile (\(zr, zi) -> zr * zr + zi * zi < 4) $ take maxIter $ iterate mandel (0, 0)
  where
    mandel (zr, zi) = (zr * zr - zi * zi + x, 2 * zr * zi + y)

-- Función que determina si un punto pertenece al conjunto de Julia
julia :: Float -> Float -> Int
julia x y = length $ takeWhile (\(zr, zi) -> zr * zr + zi * zi < 4) $ take maxIter $ iterate juliaIter (x, y)
  where
    juliaIter (zr, zi) = (zr * zr - zi * zi - 0.7, 2 * zr * zi + 0.27015)

-- Burning Ship fractal
burningShip :: Float -> Float -> Int
burningShip x y = length $ takeWhile (\(zr, zi) -> zr * zr + zi * zi < 4) $ take maxIter $ iterate burnIter (0, 0)
  where
    burnIter (zr, zi) = (zr * zr - zi * zi + x, 2 * abs zr * abs zi + y)

-- Tricorn fractal
tricorn :: Float -> Float -> Int
tricorn x y = length $ takeWhile (\(zr, zi) -> zr * zr + zi * zi < 4) $ take maxIter $ iterate tricIter (0, 0)
  where
    tricIter (zr, zi) = (zr * zr - zi * zi + x, -2 * zr * zi + y)

-- Newton fractal
newton :: Float -> Float -> Int
newton x y = length $ take maxIter $ takeWhile (\(zr, zi) -> abs (zr * zr + zi * zi - 1) > 0.0001) $ iterate newtonIter (x, y)
  where
    newtonIter (zr, zi) = (zr - (zr^3 - 3 * zr * zi^2 - 1) / (3 * zr^2 - 3 * zi^2), zi - (3 * zr^2 * zi - zi^3) / (3 * zr^2 - 3 * zi^2))

-- Phoenix fractal
phoenix :: Float -> Float -> Int
phoenix x y = length $ takeWhile (\(zr, zi, p) -> zr * zr + zi * zi < 4) $ take maxIter $ iterate phoenixIter (0, 0, 0)
  where
    phoenixIter (zr, zi, p) = (zr * zr - zi * zi + x + p * 0.5667, 2 * zr * zi + y, zr)

-- Sierpinski fractal
sierpinski :: Float -> Float -> Int
sierpinski x y | odd ((round x :: Int) .&. (round y :: Int)) = maxIter
               | otherwise = 0


-- Convertir número de iteraciones a color
colorize :: Int -> Color
colorize iter | iter == maxIter = black
              | otherwise = makeColor (norm iter) (norm (iter `div` 2)) (norm (iter `div` 3)) 1.0
  where norm i = fromIntegral i / fromIntegral maxIter

-- Selección del fractal
drawFractal :: World -> Picture
drawFractal (World zoom offX offY fractal) = Pictures [translate (fromIntegral x - fromIntegral width / 2) (fromIntegral y - fromIntegral height / 2) $ Color (colorize $ fractalFunc cx cy) $ rectangleSolid 1 1
    | x <- [0..width], y <- [0..height]
    , let cx = (fromIntegral x - fromIntegral width / 2) * zoom + offX
    , let cy = (fromIntegral y - fromIntegral height / 2) * zoom + offY ]
  where
    fractalFunc = case fractal of
                    Mandelbrot -> mandelbrot
                    Julia -> julia
                    Tricorn -> tricorn
                    BurningShip -> burningShip
                    Newton -> newton
                    Phoenix -> phoenix
                    Sierpinski -> sierpinski

-- Manejo de eventos para zoom, movimiento y cambio de fractal
handleEvent :: Event -> World -> World
handleEvent (EventKey (Char '+') Down _ _) w = w { zoomFactor = zoomFactor w * 0.9 }
handleEvent (EventKey (Char '-') Down _ _) w = w { zoomFactor = zoomFactor w * 1.1 }
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) w = w { offsetX = offsetX w - 0.1 * zoomFactor w }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) w = w { offsetX = offsetX w + 0.1 * zoomFactor w }
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) w = w { offsetY = offsetY w + 0.1 * zoomFactor w }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) w = w { offsetY = offsetY w - 0.1 * zoomFactor w }
handleEvent (EventKey (Char 'm') Down _ _) w = w { fractalType = Mandelbrot }
handleEvent (EventKey (Char 'j') Down _ _) w = w { fractalType = Julia }
handleEvent (EventKey (Char 'b') Down _ _) w = w { fractalType = BurningShip }
handleEvent (EventKey (Char 't') Down _ _) w = w { fractalType = Tricorn }
handleEvent (EventKey (Char 'n') Down _ _) w = w { fractalType = Newton }
handleEvent (EventKey (Char 'p') Down _ _) w = w { fractalType = Phoenix }
handleEvent (EventKey (Char 's') Down _ _) w = w { fractalType = Sierpinski }
handleEvent _ w = w

-- No se necesita actualizar el estado
updateWorld :: Float -> World -> World
updateWorld _ = id

-- Main
main :: IO ()
main = play (InWindow "Fractal Viewer" (width, height) (100, 100)) black 30 (World 0.005 0 0 Mandelbrot) drawFractal handleEvent updateWorld
