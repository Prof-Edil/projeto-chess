module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Maybe (catMaybes)
import Rendering
import Game
import Logic

gfxFiles :: [String]
gfxFiles = map (("images/"++) . (++".png")) $ (map ("b"++) fs) ++ (map ("w"++) fs) ++ ["promob","promow","winnerb","winnerw","draw"] where
           fs = ["k","r","b","q","n","p"]

loadGraphics :: IO [Picture]
loadGraphics = do gfx <- mapM loadJuicy gfxFiles
                  return (catMaybes gfx)
     
main = do gfx <- loadGraphics
          play (InWindow "Chess" (600, 650) (0,0)) white 30 initialGame (drawWorld gfx 500) transformGame (const id)