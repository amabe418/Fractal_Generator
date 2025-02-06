module Fractals where

import Graphics.Gloss
import Data.Complex
import Types
import ColorSchemes
import qualified Data.Vector as V
import Control.Parallel.Strategies
import Control.Parallel
import Data.List.Split (chunksOf)
import Control.DeepSeq

-- Define NFData instance for Picture
instance NFData Picture where
    rnf Blank = ()
    rnf (Polygon pts) = rnf pts
    rnf (Line pts) = rnf pts
    rnf (Circle r) = rnf r
    rnf (ThickCircle r t) = rnf r `seq` rnf t
    rnf (Arc a1 a2 r) = rnf a1 `seq` rnf a2 `seq` rnf r
    rnf (ThickArc a1 a2 r t) = rnf a1 `seq` rnf a2 `seq` rnf r `seq` rnf t
    rnf (Text str) = rnf str
    rnf (Bitmap bmp) = ()   
    rnf (Color _ pic) =  rnf pic
    rnf (Translate x y pic) = rnf x `seq` rnf y `seq` rnf pic
    rnf (Rotate a pic) = rnf a `seq` rnf pic
    rnf (Scale x y pic) = rnf x `seq` rnf y `seq` rnf pic
    rnf (Pictures pics) = rnf pics

-- Main fractal generation with parallel processing
generateFractal :: FractalState -> Picture
generateFractal state = Pictures $
    case fractalType state of
        Mandelbrot -> generateMandelbrotParallel state
        Julia -> generateJuliaParallel state
        Tree -> [generateAnimatedTree state]
        Sierpinski -> [generateAnimatedSierpinski state]
        KochSnowflake -> [generateAnimatedKoch state]
        BurningShip -> generateBurningShipParallel state

-- Parallel implementation for Mandelbrot set
generateMandelbrotParallel :: FractalState -> [Picture]
generateMandelbrotParallel state = 
    let chunks = chunksOf 100 [(x, y) | x <- [-400..400], y <- [-300..300]]
        processChunk chunk = parMap rdeepseq (generatePoint state) chunk
    in concat $ map processChunk chunks
    where
        generatePoint state (x, y) =
            let scaledX = fromIntegral x / zoom state + fst (center state)
                scaledY = fromIntegral y / zoom state + snd (center state)
                c = scaledX :+ scaledY
                iters = mandelbrotSet (maxIterations state) c
                color = getColorFunc (colorScheme state) 
                        (maxIterations state) iters (animationTime state)
            in Translate (fromIntegral x) (fromIntegral y) $
               Color color $ rectangleSolid 1 1

-- Parallel implementation for Julia set
generateJuliaParallel :: FractalState -> [Picture]
generateJuliaParallel state = 
    let chunks = chunksOf 100 [(x, y) | x <- [-400..400], y <- [-300..300]]
        processChunk chunk = parMap rdeepseq (generatePoint state) chunk
    in concat $ map processChunk chunks
    where
        generatePoint state (x, y) =
            let scaledX = fromIntegral x / zoom state + fst (center state)
                scaledY = fromIntegral y / zoom state + snd (center state)
                z = scaledX :+ scaledY
                animatedC = juliaC state * exp (0 :+ animationTime state * 0.5)
                iters = juliaSet (maxIterations state) animatedC z
                color = getColorFunc (colorScheme state) 
                        (maxIterations state) iters (animationTime state)
            in Translate (fromIntegral x) (fromIntegral y) $
               Color color $ rectangleSolid 1 1

-- Parallel implementation for Burning Ship fractal
generateBurningShipParallel :: FractalState -> [Picture]
generateBurningShipParallel state = 
    let chunks = chunksOf 100 [(x, y) | x <- [-400..400], y <- [-300..300]]
        processChunk chunk = parMap rdeepseq (generatePoint state) chunk
    in concat $ map processChunk chunks
    where
        generatePoint state (x, y) =
            let scaledX = fromIntegral x / zoom state + fst (center state)
                scaledY = fromIntegral y / zoom state + snd (center state)
                c = scaledX :+ scaledY
                iters = burningShipSet (maxIterations state) c
                color = getColorFunc (colorScheme state) 
                        (maxIterations state) iters (animationTime state)
            in Translate (fromIntegral x) (fromIntegral y) $
               Color color $ rectangleSolid 1 1

-- Animated tree with dynamic movement
generateAnimatedTree :: FractalState -> Picture
generateAnimatedTree state = 
    let time = animationTime state
        windEffect = 15 * sin (time * 2)  -- Wind animation
        scale = 1 + 0.1 * sin (time * 0.5)  -- Breathing effect
    in Translate 0 (-200) $ 
       Scale scale scale $
       Color (makeColor 0.5 0.3 0.1 1) $
       drawAnimatedTree (treeDepth state) (100 * zoom state) (30 + windEffect) time

drawAnimatedTree :: Int -> Float -> Float -> Float -> Picture
drawAnimatedTree 0 _ _ _ = Blank
drawAnimatedTree n len angle time = Pictures
    [ Line [(0, 0), (0, len)]
    , Translate 0 len $ Rotate (angle + sway) $ Scale 0.7 0.7 $ 
      drawAnimatedTree (n-1) len angle time
    , Translate 0 len $ Rotate (-(angle + sway)) $ Scale 0.7 0.7 $ 
      drawAnimatedTree (n-1) len angle time
    ]
    where sway = 5 * sin (time * 2 + fromIntegral n)  -- Different sway for each level

-- Animated Sierpinski triangle with rotation and scaling
generateAnimatedSierpinski :: FractalState -> Picture
generateAnimatedSierpinski state =
    let time = animationTime state
        rotation = 30 * sin (time * 0.5)
        scale = 1 + 0.1 * sin time
    in Rotate rotation $ Scale scale scale $
       Color (makeColor 0 1 0 1) $
       Pictures [Line [p1, p2] | (p1, p2) <- pairs (sierpinskiPoints (treeDepth state))]
    where pairs (x:y:rest) = (x,y) : pairs rest
          pairs _ = []

-- Generate points for Sierpinski triangle
sierpinskiPoints :: Int -> [Point]
sierpinskiPoints depth = go depth [(0, 200), (-200, -100), (200, -100)]
    where
        go 0 points = points
        go n points = go (n-1) (concatMap subdivide (triplets points))
        
        triplets (a:b:c:rest) = (a,b,c) : triplets rest
        triplets _ = []
        
        subdivide (p1, p2, p3) = 
            [ p1, midpoint p1 p2
            , midpoint p1 p2, p2
            , p2, midpoint p2 p3
            , midpoint p2 p3, p3
            , p3, midpoint p3 p1
            , midpoint p3 p1, p1
            ]
        
        midpoint (x1, y1) (x2, y2) = 
            ((x1 + x2) / 2, (y1 + y2) / 2)

-- Animated Koch snowflake with color cycling
generateAnimatedKoch :: FractalState -> Picture
generateAnimatedKoch state =
    let time = animationTime state
        hue = (sin time + 1) / 2
    in Color (hsvaColor hue 1 1 1) $
       Rotate (45 * sin (time * 0.3)) $
       Pictures [Line [p1, p2] | (p1, p2) <- kochLines (treeDepth state)]

-- Generate lines for Koch snowflake
kochLines :: Int -> [(Point, Point)]
kochLines depth = 
    let side = 200  -- Size of the snowflake
        height = side * sqrt 3 / 2
        top = (0, height / 2)
        bottomLeft = (-side/2, -height/2)
        bottomRight = (side/2, -height/2)
        initialLines = [(top, bottomLeft), (bottomLeft, bottomRight), (bottomRight, top)]
    in concatMap (kochLine depth) initialLines
    where
        kochLine 0 (start, end) = [(start, end)]
        kochLine n (start@(x1, y1), end@(x2, y2)) =
            let thirdX = (x2 - x1) / 3
                thirdY = (y2 - y1) / 3
                p1 = (x1, y1)
                p2 = (x1 + thirdX, y1 + thirdY)
                p3 = ( x1 + thirdX * 1.5 - thirdY * sqrt 3 / 2
                    , y1 + thirdY * 1.5 + thirdX * sqrt 3 / 2
                    )
                p4 = (x1 + 2 * thirdX, y1 + 2 * thirdY)
                p5 = (x2, y2)
            in concatMap (kochLine (n-1))
                [ (p1, p2)
                , (p2, p3)
                , (p3, p4)
                , (p4, p5)
                ]

-- Helper function for HSV to RGB conversion
hsvaColor :: Float -> Float -> Float -> Float -> Color
hsvaColor h s v a = makeColor r g b a
    where
        hi = floor (h * 6) `mod` 6
        f = h * 6 - fromIntegral hi
        p = v * (1 - s)
        q = v * (1 - f * s)
        t = v * (1 - (1 - f) * s)
        (r,g,b) = case hi of
            0 -> (v,t,p)
            1 -> (q,v,p)
            2 -> (p,v,t)
            3 -> (p,q,v)
            4 -> (t,p,v)
            _ -> (v,p,q)

-- Optimized set calculations
mandelbrotSet :: Int -> Complex Float -> Int
mandelbrotSet maxIter c = go 0 0
    where
        go n z | n >= maxIter = n
              | magnitude z > 2 = n
              | otherwise = go (n+1) (z*z + c)

juliaSet :: Int -> Complex Float -> Complex Float -> Int
juliaSet maxIter c z = go 0 z
    where
        go n w | n >= maxIter = n
              | magnitude w > 2 = n
              | otherwise = go (n+1) (w*w + c)

burningShipSet :: Int -> Complex Float -> Int
burningShipSet maxIter c = go 0 0
    where
        go n z | n >= maxIter = n
              | magnitude z > 2 = n
              | otherwise = go (n+1) ((abs(realPart z) :+ abs(imagPart z))*z + c)