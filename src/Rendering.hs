module Rendering where

import Graphics.Gloss
import Board
import Pieces
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
      color (greyN 0.5) $ scale (w/8) (w/8) $ translate 0.5 0.5 $ pictures $ (border : blackSquares)


drawWorld :: [Picture] -> Float -> Picture
drawWorld gfx bw = 
    translate (-bw*0.5) (-bw*0.55) $ pictures $ [ drawBoard bw
                                   , drawPieces bw gfx initialBoard
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

handleEvent :: (Float, Float) -> (Int, Int)
handleEvent (x, y) = ( floor ((y + (boardHeight * 0.5)) / cellHeight)
                             , floor ((x + (boardWidth * 0.5)) / cellWidth)
                             )
  where
    boardHeight = 500 
    boardWidth  = 500 
    cellHeight = boardHeight / 8 
    cellWidth  = boardWidth / 8  