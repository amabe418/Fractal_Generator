{-# LANGUAGE BangPatterns #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.Complex

-- Tamaño de la ventana
width, height :: Int
width = 1920
height = 1080

-- Límite de iteraciones
maxIter :: Int
maxIter = 100

-- Tipos de fractales
data FractalType = Mandelbrot | Julia | BurningShip | Tricorn | Newton | Julia2 | Perpendicular | Sierpinski
                 deriving (Eq, Show)

-- Definir el estado de la aplicación
data World = World { zoomFactor :: Float
                   , offsetX :: Float
                   , offsetY :: Float
                   , fractalType :: FractalType
                   }

-- Función para Mandelbrot
mandelbrot :: Float -> Float -> Int
mandelbrot x y = length $ takeWhile (\z -> magnitude z < 2) $ take maxIter $ iterate (\z -> z^2 + c) (0 :+ 0)
  where c = x :+ y

-- Función para Julia
julia :: Float -> Float -> Int
julia x y = length $ takeWhile (\z -> magnitude z < 2) $ take maxIter $ iterate (\z -> z^2 + ((-0.7) :+ 0.27015)) (x :+ y)

-- Burning Ship fractal
burningShip :: Float -> Float -> Int
burningShip x y = length $ takeWhile (\z -> magnitude z < 2) $ take maxIter $ iterate (\z -> (abs (realPart z) :+ abs (imagPart z))^2 + c) (0 :+ 0)
  where c = x :+ (-y)

-- Tricorn fractal
tricorn :: Float -> Float -> Int
tricorn x y = length $ takeWhile (\z -> magnitude z < 2) $ take maxIter $ iterate (\z -> conjugate (z^2) + c) (0 :+ 0)
  where c = x :+ y

-- Newton fractal
newton :: Float -> Float -> Int
newton x y = length $ takeWhile (\z -> magnitude (z^3 - 1) > 0.001) $ take maxIter $ iterate (\z -> z - (z^3 - 1) / (3 * z^2)) (x :+ y)

-- Julia2 fractal
julia2 :: Float -> Float -> Int
julia2 x y = length $ takeWhile (\z -> magnitude z < 2) $ take maxIter $ iterate (\z -> z^2 + ((0.285) :+ 0.01)) (x :+ y)

-- Fractal Perpendicular (nuevo)
perpendicular :: Float -> Float -> Int
perpendicular x y = length $ takeWhile (\z -> magnitude z < 2) $ take maxIter $ iterate (\z -> z^4 + c) (0 :+ 0)
  where c = x :+ y

-- Triángulo de Sierpinski
sierpinski :: Float -> Float -> Int
sierpinski x y = sierpinskiIter 0 x y
  where
    sierpinskiIter :: Int -> Float -> Float -> Int
    sierpinskiIter n x y
      | n >= maxIter = n
      | abs (x - y) < 0.1 = n  -- Cuando se acerca lo suficiente al vértice, termina
      | otherwise = sierpinskiIter (n + 1) (x / 2) (y / 2) -- Recurre en el triángulo

-- Colorea en base a las iteraciones
colorize :: Int -> Color
colorize n
  | n == maxIter = black  -- Si alcanza el máximo de iteraciones, es negro
  | otherwise = makeColorI (mod (n * 9) 255) (mod (n * 4) 255) (mod (n * 16) 255) 255

-- Selección del fractal
drawFractal :: World -> Picture
drawFractal (World zoom offX offY fractal) = Pictures
  [ translate (fromIntegral x - fromIntegral width / 2) (fromIntegral y - fromIntegral height / 2)
    $ Color (colorize $ fractalFunc cx cy) $ rectangleWire 1 1  -- Usamos rectangleWire para los bordes
  | x <- [0..width], y <- [0..height]
  , let cx = (fromIntegral x - fromIntegral width / 2) * zoom + offX
  , let cy = (fromIntegral y - fromIntegral height / 2) * zoom + offY ]
  where
    fractalFunc cx cy = case fractal of
                          Mandelbrot -> mandelbrot cx cy
                          Julia -> julia cx cy
                          Tricorn -> tricorn cx cy
                          BurningShip -> burningShip cx cy
                          Newton -> newton cx cy
                          Julia2 -> julia2 cx cy
                          Perpendicular -> perpendicular cx cy
                          Sierpinski -> sierpinski cx cy

-- Manejo de eventos (zoom, movimiento, cambio de fractal y cerrar ventana)
handleEvent :: Event -> World -> World
handleEvent (EventKey (Char '+') Down _ _) w = w { zoomFactor = zoomFactor w * 0.9 }
handleEvent (EventKey (Char '-') Down _ _) w = w { zoomFactor = zoomFactor w * 1.1 }
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) w = w { offsetX = offsetX w - 0.1 * zoomFactor w }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) w = w { offsetX = offsetX w + 0.1 * zoomFactor w }
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) w = w { offsetY = offsetY w + 0.1 * zoomFactor w }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) w = w { offsetY = offsetY w - 0.1 * zoomFactor w }
handleEvent (EventKey (Char key) Down _ _) w =
  case key of
    '1' -> w { fractalType = Mandelbrot }
    '2' -> w { fractalType = Julia }
    '3' -> w { fractalType = BurningShip }
    '4' -> w { fractalType = Tricorn }
    '5' -> w { fractalType = Newton }
    '6' -> w { fractalType = Julia2 }
    '7' -> w { fractalType = Perpendicular }
    '8' -> w { fractalType = Sierpinski }
    _   -> w
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) _ = error "Cerrar la ventana"  -- Cerrar la ventana al presionar Esc
handleEvent _ w = w

-- No se necesita actualizar el estado
updateWorld :: Float -> World -> World
updateWorld _ = id

-- Main
main :: IO ()
main = play (InWindow "Fractal Viewer" (width, height) (100, 100)) black 30 (World 0.005 0 0 Mandelbrot) drawFractal handleEvent updateWorld
