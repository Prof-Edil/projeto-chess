module Pieces where

import qualified Data.Map as Map 
import Data.Char

type Board = Map.Map (Int, Int) Piece

data Piece = Piece PColor PType
    deriving(Eq)

data PColor = White | Black
    deriving(Show,Eq)

data PType = Pawn | Knight | Rook | Bishop | Queen | King
    deriving(Show,Eq)


instance Show Piece where
    show (Piece Black Pawn)   = "p"
    show (Piece Black Rook)   = "r"
    show (Piece Black Bishop) = "b"
    show (Piece Black Queen)  = "q"
    show (Piece Black King)   = "k"
    show (Piece Black Knight) = "n"
    show (Piece _ x) = toUpper  <$> (show $ Piece Black x)

notColor Black = White
notColor White = Black

move :: (Int,Int) -> (Int,Int) -> Board -> Maybe Board
move x y board = if inBoard x && inBoard y then aux x y board else Nothing
    where aux x y board = movePiece (Map.lookup x board) x y board

moveByTurn ::  PColor -> (Int,Int) -> (Int,Int) -> Board -> Maybe Board
moveByTurn color x y board = if inBoard x && inBoard y then aux x y board else Nothing
    where aux x y board = movePieceByTurn color (Map.lookup x board) x y board

inBoard :: (Int,Int) -> Bool
inBoard (x,y) = 0 < x && x < 9 && 0 < y && y < 9

-- Piece Movements --
movePieceByTurn :: PColor -> Maybe Piece -> (Int,Int) -> (Int,Int) -> Board -> Maybe Board
movePieceByTurn color (Just (Piece c t)) x y b = if color == c then movePiece (Just (Piece c t)) x y b else Nothing
movePieceByTurn _ Nothing _ _ _ = Nothing

movePiece :: Maybe Piece -> (Int,Int) -> (Int,Int) -> Board -> Maybe Board
movePiece Nothing _ _ _ = Nothing

movePiece (Just (Piece color Pawn) ) x y board 
    | conditionEnPassant color board x y = auxMoveEnPassant x y (Piece color Pawn) board
    | conditionPawnFirstMove color board x y = auxMoveShow' x y (Piece color Pawn) board
    | otherwise = auxMoveShow (conditionPawn color board) x y (Piece color Pawn) board
    where
        conditionPawn Black board (a, b) (c, d) = 
            ((c, d) == (a+1, b) 
            && (isEmptySpace (c,d) board))
            || (isEnemyPiece Black (c,d) board 
            && ((c, d) == (a+1, b+1) || (c,d) == (a+1, b-1)))
        conditionPawn White board (a, b) (c, d) =
            (c, d) == (a-1, b) 
            && (isEmptySpace (c,d) board)
            || (isEnemyPiece White (c,d) board 
            && ((c, d) == (a-1, b-1) || (c,d) == (a-1, b+1)))
        auxMoveEnPassant (a,b) (c,d) p board = Just $ (Map.insert (c,d) p) . (Map.delete (a,b)) . (Map.delete (a,d)) $ board
        conditionEnPassant White board (a,b) (c,d) 
            = ((a == 4) && (c == 3))                  -- 5th row moving to 6th
            && ((b == d+1) || (b == d-1))             -- diagonal move
            && (isEnemyPawn White (a,d) board)        -- enemy pawn in the side cell
            && (isEmptySpace (c,d) board)             -- two empty cells behind enemy pawn (maybe moved two cells last turn)
            && (isEmptySpace (c,d-1) board)
        conditionEnPassant Black board (a,b) (c,d) 
            = ((a == 5) && (c == 6))                  -- 4th row moving to 3rd
            && ((b == d+1) || (b == d-1))             -- diagonal move
            && (isEnemyPawn Black (a,d) board)        -- enemy pawn in the side cell
            && (isEmptySpace (c,d) board)             -- two empty cells behind enemy pawn (maybe moved two cells last turn)
            && (isEmptySpace (c,d+1) board)
        conditionPawnFirstMove White board (a,b) (c,d)
            = (a == 7)                                -- initial row
            && (c == 5)                               -- trying to move two cells ahead
            && (b == d)                               -- in the same column
            && (isEmptySpace (a-1,b) board)           -- two empty cells to move ahead
            && (isEmptySpace (a-2,b) board)
        conditionPawnFirstMove Black board (a,b) (c,d)
            = (a == 2)                                -- initial row
            && (c == 4)                               -- trying to move two cells ahead
            && (b == d)                               -- in the same column
            && (isEmptySpace (a+1,b) board)           -- two empty cells to move ahead
            && (isEmptySpace (a+2,b) board)

movePiece (Just (Piece color Rook) ) x y board = auxMoveShow (conditionRook color board) x y (Piece color Rook) board

movePiece (Just (Piece color Bishop) ) x y board = auxMoveShow (conditionBishop color board) x y (Piece color Bishop) board

movePiece (Just (Piece color Queen) ) x y board = auxMoveShow (conditionQueen color board) x y (Piece color Queen) board
    where conditionQueen color board x y = conditionRook color board x y || conditionBishop color board x y

movePiece (Just (Piece color King) ) x y board 
    | conditionShortCastling color x y board = castling color x y board
    | conditionLongCastling  color x y board = castling color x y board
    | otherwise = auxMoveShow conditionKing x y (Piece color King) board
    where
        conditionKing (a, b) (c, d) = max (abs $ a-c) (abs $ b-d) == 1 && not (isAllyPiece color (c,d) board)
        conditionShortCastling Black x y board
            =  x ==  (1,5)                         -- king at the starting position
            && isAllyRook Black (1,8) board        -- tower at the starting position
            && y == (1,7)                          -- king tries to move 2 cells towards the rook
            && isEmptySpace (1,6) board            -- both spaces between them are empty cells
            && isEmptySpace (1,7) board            -- same idea was used for whites and long castling conditions ahead --
        conditionShortCastling White x y board
            =  x == (8,5)
            && isAllyRook White (8,8) board
            && y == (8,7)
            && isEmptySpace (8,6) board
            && isEmptySpace (8,7) board
        conditionLongCastling Black x y board
            =  x == (1,5)
            && isAllyRook Black (1,1) board
            && y == (1,3)
            && isEmptySpace (1,4) board
            && isEmptySpace (1,3) board
            && isEmptySpace (1,2) board
        conditionLongCastling White x y board
            =  x == (8,5)
            && isAllyRook White (8,1) board
            && y == (8,3)
            && isEmptySpace (8,4) board
            && isEmptySpace (8,3) board
            && isEmptySpace (8,2) board
        castling color x y board
            | x == (1,5) && y == (1,7) = Just
                                         $ (Map.insert (1,7) (Piece Black King))
                                         . (Map.insert (1,6) (Piece Black Rook))
                                         . (Map.delete (1,5)) . (Map.delete (1,8))
                                         $ board
            | x == (1,5) && y == (1,3) = Just 
                                         $ (Map.insert (1,3) (Piece Black King)) 
                                         . (Map.insert (1,4) (Piece Black Rook)) 
                                         . (Map.delete (1,5)) . (Map.delete (1,1))
                                         $ board
            | x == (8,5) && y == (8,7) = Just 
                                         $ (Map.insert (8,7) (Piece White King))
                                         . (Map.insert (8,6) (Piece White Rook)) 
                                         . (Map.delete (8,5)) . (Map.delete (8,8))
                                         $ board
            | x == (8,5) && y == (8,3) = Just
                                         $ (Map.insert (8,3) (Piece White King)) 
                                         . (Map.insert (8,4) (Piece White Rook)) 
                                         . (Map.delete (8,5)) . (Map.delete (8,1))
                                         $ board
            | otherwise = Nothing                                        -- this should never happen


movePiece (Just (Piece color Knight) ) x y board = auxMoveShow (conditionKnight color board) x y (Piece color Knight) board
    where conditionKnight color board (a, b) (c, d) = (a-c)^2 + (b-d)^2 == 5 && not (isAllyPiece color (c,d) board)

-- Auxiliar Functions --
auxMoveShow f x y p board = if f x y then Just $ (Map.insert y p). (Map.delete x) $ board else Nothing

auxMoveShow' x y p board = Just $ (Map.insert y p) . (Map.delete x) $ board  -- same operation, but without the condition

isAllyPiece color y board = auxAllyPiece color (Map.lookup y board)
    where auxAllyPiece color (Just (Piece color' _)) = color == color'
          auxAllyPiece color Nothing = False
          
isEmptySpace y board = if (Map.lookup y board) == Nothing then True else False
    
isEnemyPiece color y board = auxEnemyPiece color (Map.lookup y board)
    where auxEnemyPiece color (Just (Piece color' _)) = color /= color'
          auxEnemyPiece color Nothing = False

isAllyRook color y board = auxAllyPiece color (Map.lookup y board)
    where
        auxAllyPiece color (Just (Piece color' Rook)) = color == color'
        auxAllyPiece color (Just (Piece color' _)) = False
        auxAllyPiece color Nothing = False

isEnemyPawn color y board = auxEnemyPiece color (Map.lookup y board)
    where
        auxEnemyPiece color (Just (Piece color' Pawn)) = color /= color'
        auxEnemyPiece color (Just (Piece color' _)) = False
        auxEnemyPiece color Nothing = False


-- Rook Movement Condition Function --
conditionRook color board (a, b) (c, d)
    | (a,b) == (c,d) = False
    | a == c = not (isAllyPiece color (c, d) board ) && auxConditionRook1 a b d board
    | b == d = not (isAllyPiece color (c, d) board ) && auxConditionRook2 b a c board
    | otherwise = False
    
    where 
        auxConditionRook1 a b d board = 
            if d > b 
                then firstPosition1 a b [b+1 .. d-1] board 
                else firstPosition1 a b [d+1 .. b-1] board
        
        firstPosition1 a b list board 
            | null list                         = True
            | Map.member (a, (head list)) board = False
            | otherwise                         = firstPosition1 a b (tail list) board
            
        auxConditionRook2 a b d board = 
            if d > b 
                then firstPosition2 a b [b+1 .. d-1] board 
                else firstPosition2 a b [d+1 .. b-1] board
                
        firstPosition2 a b list board 
            | null list = True
            | Map.member ((head list), a) board = False
            | otherwise = firstPosition2 a b (tail list) board

-- Bishop Movement Condition Function --
conditionBishop color board (a, b) (c, d)
    | (a,b) == (c,d) = False
    | (a-c)+(b-d) == 0 = not (isAllyPiece color (c, d) board ) && auxConditionBishop1 a b d board
    | (a-c)-(b-d) == 0  = not (isAllyPiece color (c, d) board ) && auxConditionBishop2 a b d board
    | otherwise = False
    
    where 
        auxConditionBishop1 a b d board = 
            if d > b 
                then firstPosition3 a b 1 [1 .. d-b-1] board 
                else firstPosition3 a b (-1) [1 .. b-d-1] board
          
        firstPosition3 a b aux list board 
            | null list = True
            | Map.member (a - aux * (head list) ,b + aux * (head list)) board = False
            | otherwise = firstPosition3 a b aux (tail list) board

        auxConditionBishop2 a b d board = 
            if d > b 
                then firstPosition4 a b 1 [1 .. d-b-1] board 
                else firstPosition4 a b (-1) [1 .. b-d-1] board
          
        firstPosition4 a b aux list board 
            | null list = True
            | Map.member (a+ aux*(head list),b + aux*(head list)) board = False
            | otherwise = firstPosition4 a b aux (tail list) board

