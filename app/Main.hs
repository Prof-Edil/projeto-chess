module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Maybe (catMaybes)
import Rendering

gfxFiles :: [String]
gfxFiles = map (("images/"++) . (++".png")) $ (map ("b"++) fs) ++ (map ("w"++) fs) where
           fs = ["k","r","b","q","n","p"]

loadGraphics :: IO [Picture]
loadGraphics = do gfx <- mapM loadJuicy gfxFiles
                  return (catMaybes gfx)

main =  do gfx <- loadGraphics
           display (InWindow "Chess" (600, 650) (0,0)) white (drawWorld gfx 500)       