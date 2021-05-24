module Game where

import Board
import Pieces

data State = Running | Promoting (PColor,(Int,Int)) | GameOver (Maybe PColor) deriving(Eq,Show)


data Game  = Game { board :: Board
                    , selecting :: Maybe (Int, Int)
                    , current :: PColor 
                    , state :: State}

initialGame = Game { board = initialBoard
                      , selecting = Nothing
                      , current = White
                      , state = Running
                      }