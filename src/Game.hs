module Game where

import Board


data Marker  = Marker { position :: Pos
                      , selected :: Maybe Pos }

data Player  = Player { name :: String
                      , timeLeft :: Float
                      , col :: PColor
                      , captured :: [Piece] }

data State  = State { board :: Board
                    , marker :: Marker
                    , whitePlayer :: Player
                    , blackPlayer :: Player
                    , current :: PColor 
                    , message :: String }