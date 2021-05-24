module Logic where

import Game
import Graphics.Gloss.Interface.Pure.Game
import Pieces
import Board
import qualified Data.Map as Map 
import Data.Maybe (isJust)

switchPlayer game =
    case current game of
      Black -> game { current = White }
      White -> game { current = Black }

playerTurn :: Game -> (Int,Int) -> Game
playerTurn game pos
    | Just c <- winner (board game) = game {state = GameOver $ Just c}
    | (selecting game) == Nothing = game {selecting = Just pos}
    | isJust $ selecting game = auxPlayerTurn (moveByTurn (current game) (unJust $ selecting game) pos (board game)) game
    where auxPlayerTurn z game = if z == Nothing then game {selecting = Nothing} 
                                 else ( if checkPromotion pos (unJust z) then game {board = unJust z , selecting = Nothing , state = Promoting (current game, pos)}
                                        else game {board = unJust z , selecting = Nothing , current = notColor $ current game})
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


winner :: Board -> Maybe PColor
winner board =  if foldr (||) False $ Map.map (\x -> x == (Piece White King)) board 
                       then (if foldr (||) False $ Map.map (\x -> x == (Piece Black King)) board then Nothing
                             else Just (Black))
                       else Just (White)



checkPromotion :: (Int,Int) -> Board -> Bool
checkPromotion (x,y) board
        |x == 1 = Map.lookup (x,y) board == Just (Piece White Pawn)
        |x == 8 = Map.lookup (x,y) board == Just (Piece Black Pawn)
        |otherwise = False                                       

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = 
    case state game of
        Running -> playerTurn game $ mousePosAsCellCoord mousePos
        GameOver _ -> initialGame
        Promoting _ -> game

transformGame (EventKey (Char '1') Down _ _) game = 
    case state game of
        Running -> game
        GameOver _ -> game
        Promoting (White,(x,y)) -> game {board = Map.insert (x,y) (Piece White Queen). Map.delete (x,y) $ board game
                                         ,state = Running, current = Black}
        Promoting (Black,(x,y)) -> game {board = Map.insert (x,y) (Piece Black Queen). Map.delete (x,y) $ board game
                                         ,state = Running, current = White}

transformGame (EventKey (Char '2') Down _ _) game = 
    case state game of
        Running -> game
        GameOver _ -> game
        Promoting (White,(x,y)) -> game {board = Map.insert (x,y) (Piece White Knight). Map.delete (x,y) $ board game
                                         ,state = Running, current = Black}
        Promoting (Black,(x,y)) -> game {board = Map.insert (x,y) (Piece Black Knight). Map.delete (x,y) $ board game
                                         ,state = Running, current = White}
transformGame (EventKey (Char '3') Down _ _) game = 
    case state game of
        Running -> game
        GameOver _ -> game
        Promoting (White,(x,y)) -> game {board = Map.insert (x,y) (Piece White Bishop). Map.delete (x,y) $ board game
                                         ,state = Running, current = Black}
        Promoting (Black,(x,y)) -> game {board = Map.insert (x,y) (Piece Black Bishop). Map.delete (x,y) $ board game
                                         ,state = Running, current = White}
transformGame (EventKey (Char '4') Down _ _) game = 
    case state game of
        Running -> game
        GameOver _ -> game
        Promoting (White,(x,y)) -> game {board = Map.insert (x,y) (Piece White Rook). Map.delete (x,y) $ board game
                                         ,state = Running , current = Black}
        Promoting (Black,(x,y)) -> game {board = Map.insert (x,y) (Piece Black Rook). Map.delete (x,y) $ board game
                                         ,state = Running, current = White}

transformGame _ game = game
