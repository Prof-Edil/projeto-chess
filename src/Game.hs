module Game where

import Board
import Pieces

data State = Running | Promoting (PColor,(Int,Int)) | GameOver (Maybe PColor) deriving(Eq,Show)

data Player  = Player { timeLeft :: Float
                      , captured :: [Piece] }

data Game  = Game { board :: Board
                    , selecting :: Maybe (Int, Int)
                    , whitePlayer :: Player
                    , blackPlayer :: Player
                    , current :: PColor 
                    , state :: State}

initialGame = Game { board = initialBoard
                      , selecting = Nothing
                      , whitePlayer = Player {timeLeft = 999  , captured = []}
                      , blackPlayer = Player {timeLeft = 999 , captured = []}
                      , current = White
                      , state = Running
                      }