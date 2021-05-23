module Game where

import Board
import Pieces

data State = Running | GameOver deriving(Eq,Show)

data Player  = Player { name :: String
                      , timeLeft :: Float
                      , col :: PColor
                      , captured :: [Piece] }

data Game  = Game { board :: Board
                    , selecting :: Maybe (Int, Int)
                    , whitePlayer :: Player
                    , blackPlayer :: Player
                    , current :: PColor 
                    , message :: String
                    , state :: State}

initialGame = Game { board = initialBoard 
                      , selecting = Nothing
                      , whitePlayer = Player {name = "White" , timeLeft = 999 , col = White , captured = []}
                      , blackPlayer = Player {name = "Black" , timeLeft = 999 , col = Black , captured = []}
                      , current = White
                      , message = ""
                      , state = Running
                      }