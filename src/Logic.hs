module Logic where

import Game
import Graphics.Gloss.Interface.Pure.Game
import Pieces
import Board

switchPlayer game =
    case current game of
      Black -> game { current = White }
      White -> game { current = Black }

playerTurn :: Game -> (Int,Int) -> Game
playerTurn game pos = 
    case selecting game of
        Nothing -> game {selecting = Just pos}
        Just _ -> auxPlayerTurn (moveByTurn (current game) (unJust $ selecting game) pos (board game)) game
    where auxPlayerTurn z game = if z == Nothing then game {selecting = Nothing} 
                                 else game {board = unJust z , selecting = Nothing , current = notColor $ current game}
          unJust (Just x) = x

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( 8 - floor ((y+(boardHeight * 0.5)) / cellHeight + 0.375)
                             , floor ((x + (boardWidth * 0.5)) / cellWidth + 1)
                             )
  where
    boardHeight = 500
    boardWidth  = 500
    cellHeight = boardHeight / 8
    cellWidth  = boardWidth / 8


transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = 
    playerTurn game $ mousePosAsCellCoord mousePos

transformGame _ game = game
