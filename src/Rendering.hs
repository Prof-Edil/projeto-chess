module Rendering where

import Graphics.Gloss

drawBoard :: Float -> Picture
drawBoard w = 
    let
      tup a b = (a,b)
      sqAt (c,r) = translate c r $ rectangleSolid 1 1
      squares = (tup <$> [0,2,4,6] <*> [0,2,4,6]) ++ (tup <$> [1,3,5,7] <*> [1,3,5,7])
      border = translate 3.5 3.5 $ rectangleWire 8 8
      blackSquares = map sqAt squares
    in 
      color (greyN 0.5) $ scale (w/8) (w/8) $ translate 0.5 0.5 $ pictures $ (border : blackSquares)



--loadGraphics :: IO [Picture]
--loadGraphics = do gfx <- mapM loadJuicy gfxFiles
--                  return (catMaybes gfx)

translatePos :: Float -> (Int,Int) -> (Float, Float)
translatePos bw (c, r) = ((bw/8)*fromIntegral c, (bw/8)*fromIntegral (7-r))

gfxFiles :: [String]
gfxFiles = map (("images/"++) . (++".png")) $ (map (++"b") fs) ++ (map (++"w") fs) where
           fs = ["k","r","b","q","n","p"]