module TesterCode where
import Checkers
import Data.Maybe
-- This file provides tester code for testing and debugging of our features and functions. More will be added later down the line.


-- The following Tester snippet generates a full board for the start of the game. 

blackPiece = (Black, NoKing)
redPiece = (Red, NoKing)
board :: Board
board = [((1,0), blackPiece), ((3,0), blackPiece), ((5,0), blackPiece), ((7,0), blackPiece),((0,1), blackPiece), ((2,1), blackPiece), ((4,1), blackPiece), ((6,1), blackPiece),((1,2), blackPiece), ((3,2), blackPiece), ((5,2), blackPiece), ((7,2), blackPiece),((0,5), redPiece), ((2,5), redPiece), ((4,5), redPiece), ((6,5), redPiece),((1,6), redPiece), ((3,6), redPiece), ((5,6), redPiece), ((7,6), redPiece),((0,7), redPiece), ((2,7), redPiece), ((4,7), redPiece), ((6,7), redPiece)]


-- Check all edge cases for isValidMove.


move1 = ((1,2),(2,3))

maybeLoc :: Maybe Loc
maybeLoc = Nothing
gameState1 = (Black, board, maybeLoc, 10)

-- Basic board to check whoWillWin
testBoard1 :: Board
testBoard1 = [((3,4),redPiece),((2,3),blackPiece)]

testBoard2 :: Board
testBoard2 = [((3,4),redPiece),((2,3),blackPiece),((6,5),blackPiece)]

testBoard3 :: Board
testBoard3 = [((3,4), blackPiece)]

gs2 :: GameState
gs2 = (Red, testBoard1, maybeLoc, 5)

gs3 :: GameState
gs3 = (Black, testBoard2, maybeLoc, 14)

gs4 :: GameState
gs4 = (Black, testBoard3, maybeLoc, 5)