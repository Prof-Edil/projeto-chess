module Board where

import qualified Data.Map as Map 
import Data.Char
import Pieces

-- A simulation of a game (currently without turns and the end condition is an invalid move)
-- The coordinates used are matrix like: 
--
--  11  12  13 ...
--  21  22  23 ...
--  31  32  33 ...
--  ... 
--
-- Example of a game:
-- game initial_board
-- 2 5 
-- 3 5
-- 1 4
-- 4 7
-- 4 7
-- 7 4
-- 7 4
-- 0 0

game board = do 
        putChar '\n'
        showBoard board
        a <- (\x -> read [x] :: Int) <$> getChar
        getChar
        b <- (\x -> read [x] :: Int) <$> getChar
        getChar
        putChar '\n'
        showMoves (a,b) board
        c <- (\x -> read [x] :: Int) <$> getChar
        getChar
        d <- (\x -> read [x] :: Int) <$> getChar
        getChar
        
        auxGame (move (a,b) (c,d) board)
        where 
            unJust (Just x) = x
            auxGame x = if x == Nothing then putStr "" else game (unJust x)


--initial board ---
initialBoard :: Board
initialBoard = Map.fromList [((1,1), Piece Black Rook), ((1,2), Piece Black Knight), ((1,3), Piece Black Bishop)
                            , ((1,4), Piece Black Queen), ((1,5), Piece Black King), ((1,6), Piece Black Bishop)
                            , ((1,7), Piece Black Knight), ((1,8), Piece Black Rook), ((2,1), Piece Black Pawn)
                            , ((2,2), Piece Black Pawn), ((2,3), Piece Black Pawn), ((2,4), Piece Black Pawn)
                            , ((2,5), Piece Black Pawn), ((2,6), Piece Black Pawn), ((2,7), Piece Black Pawn)
                            , ((2,8), Piece Black Pawn), ((8,1), Piece White Rook), ((8,2), Piece White Knight) 
                            , ((8,3), Piece White Bishop), ((8,4), Piece White Queen), ((8,5), Piece White King) 
                            , ((8,6), Piece White Bishop), ((8,7), Piece White Knight), ((8,8), Piece White Rook) 
                            , ((7,1), Piece White Pawn), ((7,2), Piece White Pawn), ((7,3), Piece White Pawn)
                            , ((7,4), Piece White Pawn), ((7,5), Piece White Pawn), ((7,6), Piece White Pawn)
                            , ((7,7), Piece White Pawn), ((7,8), Piece White Pawn)
                            ] :: Board

--test boards--
testBoard :: Board
testBoard = Map.fromList $ zip [(1,2),(3,3),(5,4),(2,4),(1,4),(4,3)] [Piece Black Pawn, Piece White Rook, Piece Black Bishop, Piece White Queen, Piece Black King, Piece White Knight] :: Board

testBoardPawnStucked :: Board
testBoardPawnStucked = Map.fromList $ zip [(2,1),(3,1)] [Piece Black Pawn, Piece White Pawn] :: Board

testBoardPawnEnPassant :: Board
testBoardPawnEnPassant = Map.fromList $ zip [(4,3),(4,2),(4,4),(5,7),(5,6),(5,8)] [Piece White Pawn, Piece Black Pawn, Piece Black Pawn, Piece Black Pawn, Piece White Pawn, Piece White Pawn]

-- functions with board 

boardGenerator list1 list2 = Map.fromList $ zip list1 list2

showBoard board =  putStrLn $ (auxShowBoard 1 1) . Map.toList $ board
    where 
        auxShowBoard i j [] 
            | i == j && j == 8 = " _ "
            | j < 9   = " _ " ++ (auxShowBoard i (j+1) [] ) 
            | j == 9  = "\n" ++ (auxShowBoard (i+1) 1 [] )
        auxShowBoard i j list
            | i == j && j == 8 && ((fst.head) list == (i,j)) = " " ++ (show.snd.head) list
            | i == j && j == 8 = " _ "
            | j < 9  && ((fst.head) list == (i,j)) =  " " ++ (show.snd.head) list ++ " "++ auxShowBoard i (j+1) (tail list)
            | j < 9  = " _ " ++ auxShowBoard i (j+1) list
            | j == 9  = "\n" ++ (auxShowBoard (i+1) 1 list )
    

-- Show possible moves of the given position
showMoves (x, y) board =  putStrLn $ (auxShowMoves 1 1 (x,y) board) . Map.toList $ board 
    where
        auxShowMoves i j (x, y) board [] 
            | i == j && j == 8 = stringConv (x, y) (i, j) board " _ "
            | j < 9   = stringConv (x, y) (i, j) board " _ " ++ (auxShowMoves i (j+1) (x, y) board [] ) 
            | j == 9  = "\n" ++ (auxShowMoves (i+1) 1 (x, y) board [] )
        auxShowMoves i j (x, y) board list
            | i == j && j == 8 && ((fst.head) list == (i,j)) = stringConv (x, y) (i, j) board (" " ++ (show.snd.head) list)
            | i == j && j == 8 = stringConv (x, y) (i, j) board " _ "
            | j < 9  && ((fst.head) list == (i,j)) =  stringConv (x, y) (i, j) board (" " ++ (show.snd.head) list ++ " ") ++ auxShowMoves i (j+1) (x, y) board (tail list)
            | j < 9  = stringConv (x, y) (i, j) board " _ " ++ auxShowMoves i (j+1) (x, y) board list
            | j == 9  = "\n" ++ (auxShowMoves (i+1) 1 (x, y) board list )
        
        stringConv x y board s
            | (move x y board) /= Nothing = if s == " _ " then " * " else " & "
            | x == y = "|" ++ (tail s) 
            | otherwise = s 

-- Moves the square x to square y if possible and thn show it.
moveShow x y board = auxMoveShow $ move x y board
    where 
        auxMoveShow (Just x) = showBoard x
        auxMoveShow Nothing = putStrLn "Movimento inválido"
