module Main where

import Board
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Rendering

picture1 = Translate (-250) (-250) $ drawBoard 500
--picture2 = Translate (-170) 0 $ Scale 0.5 0.5 <$> Text "Hello World" 


main =  do picture <- loadJuicy "images/wk.png"
           display (InWindow "Chess" (600, 650) (0,0)) white $ pictures [picture1,unJust picture]
        where unJust (Just x) = x