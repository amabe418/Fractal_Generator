module Main where

import Web.Scotty (scotty, post, get, json, jsonData, text, ActionM, middleware)
import Network.Wai.Middleware.Cors (simpleCors)
import Data.String (fromString)
import qualified Data.Text.Lazy as TL
import Fractal
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = scotty 3000 $ do
    middleware simpleCors

    post (fromString "/generate") $ do
    params <- jsonData :: ActionM FractalParams
    liftIO $ print params
    json $ generateFractal params

    -- Asegúrate de que el servidor responda correctamente a las solicitudes OPTIONS
    options (fromString "/generate") $ do
        text (TL.pack "OK")


    get (fromString "/health") $ do
        text (TL.pack "OK")
