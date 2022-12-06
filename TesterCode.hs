module TesterCode where
import Checkers
import Data.Maybe
-- This file provides tester code for testing and debugging of our features and functions. More will be added later down the line.


-- The following Tester snippet generates a full board for the start of the game. 

blackPiece = (Black, NoKing)
redPiece = (Red, NoKing)
blackKing = (Black, King)
redKing = (Red, King)
board :: Board
board = [((1,0), blackPiece), ((3,0), blackPiece), ((5,0), blackPiece), ((7,0), blackPiece),((0,1), blackPiece), ((2,1), blackPiece), ((4,1), blackPiece), ((6,1), blackPiece),((1,2), blackPiece), ((3,2), blackPiece), ((5,2), blackPiece), ((7,2), blackPiece),((0,5), redPiece), ((2,5), redPiece), ((4,5), redPiece), ((6,5), redPiece),((1,6), redPiece), ((3,6), redPiece), ((5,6), redPiece), ((7,6), redPiece),((0,7), redPiece), ((2,7), redPiece), ((4,7), redPiece), ((6,7), redPiece)]


-- Check all edge cases for isValidMove.


move1 = ((1,2),(2,3))

maybeLoc :: Maybe Loc
maybeLoc = Nothing
gameState1 :: GameState
gameState1 = (Black, board, maybeLoc, 10)

-- Basic board to check whoWillWin
testBoard1 :: Board
testBoard1 = [((3,4),redPiece),((2,3),blackPiece)] -- One piece middle of board

testBoard2 :: Board
testBoard2 = [((3,4),redPiece),((2,3),blackPiece),((6,5),blackPiece)] -- Two pieces black sided one piece red sided

testBoard3 :: Board
testBoard3 = [((3,4), blackPiece)]

gs2 :: GameState
gs2 = (Red, testBoard1, maybeLoc, 5)

gs3 :: GameState
gs3 = (Black, testBoard2, maybeLoc, 14)

gs3r :: GameState
gs3r = (Red, testBoard2, maybeLoc, 14)

gs3t :: GameState
gs3t = (Black, testBoard3, maybeLoc, 5)

testBoard3_5 :: Board
testBoard3_5 = [((1,1), blackPiece),((3,1), blackPiece), ((5,1), blackPiece), ((7,1), blackPiece),((0,2), blackPiece), ((2,2), blackPiece), ((4,2), blackPiece), ((6,2), blackPiece), ((1,3), blackPiece),((3,3), blackPiece), ((5,3), blackPiece), ((7,3), blackPiece),((0,4), redPiece), ((2,4), redPiece), ((4,4), redPiece), ((6,4), redPiece),((1,5), redPiece), ((3,5), redPiece), ((5,5), redPiece), ((7,5), redPiece),((0,6), redPiece), ((2,6), redPiece), ((4,6), redPiece), ((6,6), redPiece)]
-- Test board for a better developed game, with all pieces ahead by one row

gs4 :: GameState
gs4 = (Black, testBoard3_5, maybeLoc, 14)

gs4h :: GameState
gs4h = (Red, testBoard3_5, maybeLoc, 14)


testBoard4 :: Board
testBoard4 = [((2,0), blackKing), ((2,7), blackKing), ((3,3), redPiece), ((4,2), redPiece), ((5,3), redPiece)]
-- A advanced game board. We have two black kings, one at the end of the board while the other is on row two. Three red pieces remaining, close to one another, with
-- the most developed one being in 4,2, two moves away from becoming king.

gs5 :: GameState
gs5 = (Black, testBoard4, maybeLoc, 5)

gs5h :: GameState
gs5h = (Red, testBoard4, maybeLoc, 5)

testBoard5 :: Board
testBoard5 = [((3,0), redKing), ((4,1), blackPiece), ((6,3), blackPiece), ((4,5), blackPiece), ((2,5), blackPiece), ((2,3), blackPiece), ((2,1), blackPiece)]
-- A board with a redking atop capable of doing jump moves to eliminate a board of 6 black pieces. Used to debug bestmove for both sides

gs6 :: GameState
gs6 =(Black, testBoard5, maybeLoc, 5)
-- Ask about maybeLoc

gs6h :: GameState
gs6h = (Red, testBoard5, maybeLoc, 5)

testBoard6 :: Board
testBoard6 = [((7,0), blackPiece), ((5,2), blackPiece), ((7,4), blackPiece), ((5,4), blackPiece), ((6,5), redPiece), ((5,6), redPiece), ((6,7), redPiece)]
-- A board meant to see how bestMoves and Moves handle corners

gs7 :: GameState
gs7 = (Black, testBoard6, maybeLoc, 5)

gs7h :: GameState
gs7h = (Red, testBoard6, maybeLoc, 5)

