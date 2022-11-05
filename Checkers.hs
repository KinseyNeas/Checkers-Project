module Checkers where
import Data.List (sort)
import Debug.Trace
import Data.List

--                                                      Type Aliases

-- This data type is meant to represent what a tile on our board contains at any given moment.

type Piece = (Color, Class)

-- This data type differentiates the color of the pieces so that we can keep track of which
-- player is playing/check if they are moving the correct pieces.

data Color = Red | Black deriving Eq

-- This data type clarifies the type of piece being moved by the player. This will affect the limitations of the piece.

data Class = NoKing | King deriving Eq

-- This type contains a move. The first integer in the tuple is the location of the 
-- checker and the second integer is the location the player wants to move their piece.

type Loc = (Int, Int)

type Move = (Loc,Loc)

-- This type represnts our board.

type Board = [(Loc, Piece)]

type GameState = (Color, Board, Maybe Loc)


--                                                     Print Function

-- Instances of show so that player can see board and pieces.

printBoard :: GameState -> [String]
printBoard (c,board,mLoc) = [printRow board [(x,y)|x <- [0..7]] |y <- [0..7]] --(zip [0..7] y)

printRow :: Board -> [Loc] -> String
printRow _ [] = ""
printRow [] (p:ps) = "_ " ++ printRow [] ps
printRow (b:bs) (p:ps) = 
    if (fst b == p) then show (fst (snd b))++ " " ++ printRow bs ps
    else if (snd (fst b) /= snd p) then "_ " ++ printRow bs ps
    else "_ " ++ printRow (b:bs) ps
--                                                      Functions

-- Checks if a move, based on the type and color of the piece, is legal.


isValidMove :: Move -> GameState -> Bool
isValidMove = undefined

validMoves :: Move -> GameState -> [Move]
validMoves = undefined

isCapture :: Move -> GameState -> Bool
isCapture = undefined

-- Once a move is confirmed to be legal, we can update the board to reflect the new Move.

makeMove :: GameState -> Move -> Maybe GameState
makeMove = undefined

-- Function used to check if the game is over.

checkGameOver :: GameState -> Bool
checkGameOver = undefined

checkWinner :: GameState -> Color
checkWinner = undefined

--                                                      Extra Notes
--
-- Will need a counter to keep track of how many pieces each player has at a given time 
-- so that we can decide if someone has won.
-- 
-- checkCount :: Board -> Bool
-- Counts how many pieces are left on each side and if either one = 0 then it returns true 
-- to tell the computer to stop the game and declare a winner.
--
-- Will need to figure out how to make a starting board for player to play on and how to display it.
--
-- Can pattern match in isValidMove to clarify what type of checker can do what.
--
-- makeMove :: Int -> Int -> Move ?? Function that takes in player input and puts it into Move format.
-- Not sure if we need to do that here or not.
--
-- Will need to be able to identify when a player can move again.
--
-- Need a function that deals with capture.


--                                                      Rules
 
-- Setup: 
--  - 8 x 8 Game Board 
--  - 24 Pieces, 2 Players, 12 Pieces/Person
--  - Each person has light square on right side corner at start (only use dark tiles).

-- Game Play:
--  - Black starts game.
--  - Move: Pieces move diagonally, and single pieces can only move forward.
--          A piece making a non-capturing move may move only one square.

-- Capture: 
--  - To capture a piece of your opponent, your piece leaps over one of the 
--    opponent's pieces and lands in a straight diagonal line on the other side. 
--    This landing square must be empty.
--  - When a piece is captured, it is removed from the board.
--  - Only one piece may be captured in a single jump, but multiple jumps are allowed on a single turn.

-- KingMe:
--  - When a piece reaches the furthest row, it is crowned and becomes a king (double).
--  - Kings are limited to moving diagonally but can move both forward and backward.
--  - Kings may combine jumps in several directions (forward and backward) on the same turn.

-- End of Game:
--  - A player wins the game when the opponent cannot make a move.
--  - This happens usually because all of the opponent's pieces have been captured, 
--    but it could also be because all of his pieces are blocked in.




