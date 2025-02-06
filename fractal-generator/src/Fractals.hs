module Fractals where

import Graphics.Gloss
import Data.Complex
import Types
import ColorSchemes
import qualified Data.Vector as V
import Control.Parallel.Strategies

generateFractal :: FractalState -> Picture
generateFractal state = case fractalType state of
    Mandelbrot -> generateMandelbrot state
    Julia -> generateJulia state
    Tree -> generateTree state
    Sierpinski -> generateSierpinski state
    KochSnowflake -> generateKoch state
    BurningShip -> generateBurningShip state

generateMandelbrot :: FractalState -> Picture
generateMandelbrot state = Pictures
    [ Translate (fromIntegral x) (fromIntegral y) $
      Color (getColorFunc (colorScheme state) (maxIterations state) iters) $
      rectangleSolid 1 1
    | x <- [-400..400]
    , y <- [-300..300]
    , let scaledX = (fromIntegral x / zoom state + fst (center state))
    , let scaledY = (fromIntegral y / zoom state + snd (center state))
    , let c = scaledX :+ scaledY
    , let iters = mandelbrotSet (maxIterations state) c
    ]

generateJulia :: FractalState -> Picture
generateJulia state = Pictures
    [ Translate (fromIntegral x) (fromIntegral y) $
      Color (getColorFunc (colorScheme state) (maxIterations state) iters) $
      rectangleSolid 1 1
    | x <- [-400..400]
    , y <- [-300..300]
    , let scaledX = (fromIntegral x / zoom state + fst (center state))
    , let scaledY = (fromIntegral y / zoom state + snd (center state))
    , let z = scaledX :+ scaledY
    , let iters = juliaSet (maxIterations state) (juliaC state) z
    ]

generateTree :: FractalState -> Picture
generateTree state = 
    Translate 0 (-200) $ 
    Color (makeColor 0.5 0.3 0.1 1) $
    drawTree (treeDepth state) (100 * zoom state) 30

generateSierpinski :: FractalState -> Picture
generateSierpinski state = 
    Color (makeColor 0 1 0 1) $
    Pictures [Line [p1, p2] | (p1, p2) <- pairs (sierpinskiPoints (treeDepth state))]
    where pairs (x:y:rest) = (x,y) : pairs rest
          pairs _ = []

generateKoch :: FractalState -> Picture
generateKoch state =
    Color (makeColor 0 0 1 1) $
    Pictures [Line [p1, p2] | (p1, p2) <- kochLines (treeDepth state)]

generateBurningShip :: FractalState -> Picture
generateBurningShip state = Pictures
    [ Translate (fromIntegral x) (fromIntegral y) $
      Color (getColorFunc (colorScheme state) (maxIterations state) iters) $
      rectangleSolid 1 1
    | x <- [-400..400]
    , y <- [-300..300]
    , let scaledX = (fromIntegral x / zoom state + fst (center state))
    , let scaledY = (fromIntegral y / zoom state + snd (center state))
    , let c = scaledX :+ scaledY
    , let iters = burningShipSet (maxIterations state) c
    ]

mandelbrotSet :: Int -> Complex Float -> Int
mandelbrotSet maxIter c = length $ takeWhile (\z -> magnitude z <= 2) 
                       $ take maxIter 
                       $ iterate (\z -> z*z + c) 0

juliaSet :: Int -> Complex Float -> Complex Float -> Int
juliaSet maxIter c z = length $ takeWhile (\w -> magnitude w <= 2) 
                     $ take maxIter 
                     $ iterate (\w -> w*w + c) z

burningShipSet :: Int -> Complex Float -> Int
burningShipSet maxIter c = length $ takeWhile (\z -> magnitude z <= 2) 
                        $ take maxIter 
                        $ iterate (\z -> (abs(realPart z) :+ abs(imagPart z))*z + c) 0

drawTree :: Int -> Float -> Float -> Picture
drawTree 0 _ _ = Blank
drawTree n len angle = Pictures
    [ Line [(0, 0), (0, len)]
    , Translate 0 len $ Rotate angle $ Scale 0.7 0.7 $ drawTree (n-1) len angle
    , Translate 0 len $ Rotate (-angle) $ Scale 0.7 0.7 $ drawTree (n-1) len angle
    ]

sierpinskiPoints :: Int -> [(Float, Float)]
sierpinskiPoints n = iterate subdivide initialTriangle !! n
    where
        initialTriangle = [(-200, -150), (200, -150), (0, 200)]
        subdivide points = concat [[p1, midpoint p1 p2] | (p1, p2) <- zip points (tail points ++ [head points])]
        midpoint (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

kochLines :: Int -> [((Float, Float), (Float, Float))]
kochLines n = concatMap (kochLine n) initialLines
    where
        size = 200
        initialLines = [((-size, 0), (size, 0))]

kochLine :: Int -> ((Float, Float), (Float, Float)) -> [((Float, Float), (Float, Float))]
kochLine 0 line = [line]
kochLine n (p1@(x1,y1), p2@(x2,y2)) = 
    concatMap (kochLine (n-1)) [(p1,p3), (p3,p4), (p4,p5), (p5,p2)]
    where
        dx = (x2-x1)/3
        dy = (y2-y1)/3
        p3 = (x1 + dx, y1 + dy)
        p4 = (x1 + dx*1.5 - dy*0.866, y1 + dy*1.5 + dx*0.866)
        p5 = (x1 + dx*2, y1 + dy*2)


pixelToColor :: FractalState -> (Float, Float) -> Color
pixelToColor state (x, y) =
    let scaledX = (x / zoom state + fst (center state))
        scaledY = (y / zoom state + snd (center state))
        c = scaledX :+ scaledY
        colorFunc = getColorFunc (colorScheme state)
        iters = case fractalType state of
            Mandelbrot -> mandelbrotSet (maxIterations state) c
            Julia -> juliaSet (maxIterations state) (juliaC state) c
            BurningShip -> burningShipSet (maxIterations state) c
            _ -> maxIterations state -- For non-iteration based fractals
    in colorFunc (maxIterations state) iters

