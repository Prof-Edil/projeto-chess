module Rendering where

import Graphics.Gloss
import Board
import Pieces
import Game 
import qualified Data.Map as Map 

drawBoard :: Float -> Picture
drawBoard w = 
    let
      tup a b = (a,b)
      sqAt (c,r) = translate c r $ rectangleSolid 1 1
      squares = (tup <$> [0,2,4,6] <*> [0,2,4,6]) ++ (tup <$> [1,3,5,7] <*> [1,3,5,7])
      border = translate 3.5 3.5 $ rectangleWire 8 8
      blackSquares = map sqAt squares
    in 
      color (makeColorI 48 100 71 200) $ scale (w/8) (w/8) $ translate 0.5 0.5 $ pictures $ (border : blackSquares)

drawSelected :: Float -> Maybe (Int,Int) -> Picture
drawSelected w (Just (x,y)) = if inBoard (x,y) 
  then color red $ scale (w/8) (w/8) $ translate ( fromIntegral y -0.5) (8-(fromIntegral x -0.5)) $ rectangleSolid 1 1 else Blank 
drawSelected w Nothing = Blank

drawDots :: Float -> Game -> Picture
drawDots w game = if (selecting game) == Nothing then Blank else pictures $ conditionDots (board game) <$> ((\x y -> (x,y)) <$> [1..8] <*> [1..8])
                  where dots :: Int -> Int -> Picture
                        dots x y = color (greyN 0.25) $ scale (w/8) (w/8) $ translate ( fromIntegral y -0.5) (8-(fromIntegral x -0.5)) $ circleSolid 0.08
                        conditionDots :: Board -> (Int,Int) -> Picture
                        conditionDots b (x,y) = if (move (unJust $ selecting game) (x,y) b) /= Nothing then dots x y else Blank 
                        unJust (Just x) = x

drawWorld :: [Picture] -> Float -> Game -> Picture
drawWorld gfx bw game = 
    translate (-bw*0.5) (-bw*0.55) $ pictures $ [ drawBoard bw
                                   , drawSelected bw (selecting game)
                                   , drawDots bw game
                                   , drawPieces bw gfx (board game)
--                                   , Text . show $ selecting game
                                   ]

drawPieces :: Float -> [Picture] -> Board -> Picture
drawPieces bw gfx b = 
    translate (bw/16) (bw/16) $ pictures $ Map.elems $ Map.mapWithKey (\pos p -> toPic pos p) b 
    where
      toPic :: (Int,Int) -> Piece -> Picture
      toPic pos p = let (tx, ty) = translatePos bw $ pos
                    in  translate tx ty $ pieceGfx gfx p


translatePos :: Float -> (Int,Int) -> (Float, Float)
translatePos bw (c, r) = ((bw/8)*fromIntegral (r-1), (bw/8)*fromIntegral (8-c))


pieceGfx :: [Picture] -> Piece -> Picture
pieceGfx gfx p = 
    let 
      ts = [ King, Rook, Bishop, Queen, Knight, Pawn ]
      pieces = map (Piece Black) ts ++ map (Piece White) ts
      pic = lookup p $ zip pieces gfx
    in
      maybe Blank id pic 

