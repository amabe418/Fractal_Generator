{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GI.Gtk as Gtk
import qualified GI.Cairo as Cairo
import qualified GI.Gdk as Gdk
import Data.IORef
import Control.Monad (when)

import FractalTypes
import FractalLogic

main :: IO ()
main = do
  Gtk.init Nothing
  
  -- Create window
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle window "Fractal Generator"
  Gtk.setContainerBorderWidth window 10
  
  -- Create main container
  box <- Gtk.boxNew Gtk.OrientationVertical 10
  Gtk.containerAdd window box
  
  -- Create drawing area
  canvas <- Gtk.drawingAreaNew
  Gtk.widgetSetSizeRequest canvas 600 600
  
  -- Create controls
  controlBox <- Gtk.boxNew Gtk.OrientationHorizontal 5
  
  -- Fractal type selector
  typeCombo <- Gtk.comboBoxTextNew
  Gtk.comboBoxTextAppendText typeCombo "Mandelbrot"
  Gtk.comboBoxTextAppendText typeCombo "Julia"
  Gtk.comboBoxSetActive typeCombo 0
  
  -- Max iterations control
  iterScale <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 10 1000 10
  Gtk.scaleSetValue iterScale 100
  
  -- Add controls to control box
  Gtk.boxPackStart controlBox typeCombo False False 0
  Gtk.boxPackStart controlBox iterScale True True 0
  
  -- Add everything to main box
  Gtk.boxPackStart box controlBox False False 0
  Gtk.boxPackStart box canvas True True 0
  
  -- Create state reference
  stateRef <- newIORef initialState
  
  -- Drawing function
  let draw :: Cairo.Context -> IO ()
      draw cr = do
        state <- readIORef stateRef
        let width = 600
            height = 600
            
        -- Clear background
        Cairo.setSourceRGB cr 0 0 0
        Cairo.paint cr
        
        -- Draw fractal
        sequence_ 
          [ do let c = pixelToComplex (width, height) (x, y) (zoom state) (centerX state) (centerY state)
               let iters = getFractalValue state c
               let color = iterToColor iters (maxIter state)
               Cairo.setSourceRGB cr (color!!0) (color!!1) (color!!2)
               Cairo.rectangle cr (fromIntegral x) (fromIntegral y) 1 1
               Cairo.fill cr
          | x <- [0..width-1]
          , y <- [0..height-1]
          ]
  
  -- Connect signals
  Gtk.onWidgetDraw canvas $ \context -> do
    draw context
    return True
  
  Gtk.onComboBoxChanged typeCombo $ do
    active <- Gtk.comboBoxGetActive typeCombo
    modifyIORef stateRef $ \s -> s { fractalType = if active == 0 then Mandelbrot else Julia }
    Gtk.widgetQueueDraw canvas
  
  Gtk.onRangeValueChanged iterScale $ do
    value <- Gtk.rangeGetValue iterScale
    modifyIORef stateRef $ \s -> s { maxIter = floor value }
    Gtk.widgetQueueDraw canvas
  
  -- Mouse controls
  Gtk.onWidgetButtonPressEvent canvas $ \event -> do
    button <- Gdk.getEventButtonButton event
    (x, y) <- Gdk.getEventButtonCoords event
    when (button == 1) $ do  -- Left click to pan
      modifyIORef stateRef $ \s -> s 
        { centerX = centerX s + (x - 300) / 150 / zoom s
        , centerY = centerY s + (y - 300) / 150 / zoom s
        }
      Gtk.widgetQueueDraw canvas
    when (button == 3) $ do  -- Right click to zoom
      modifyIORef stateRef $ \s -> s { zoom = zoom s * 1.5 }
      Gtk.widgetQueueDraw canvas
    return True
  
  -- Show everything
  Gtk.widgetShowAll window
  Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.main