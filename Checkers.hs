module Checkers where
import Data.List (sort)
import Debug.Trace
import Data.List

--                                                      Type Alises

-- This data type is meant to represent what a tile on our board contains at any given moment.

data Fill = Empty | Checker Color Type

-- This data type differentiates the color of the pieces so that we can keep track of which
-- player is playing/check if they are moving the correct pieces.

data Color = Red | Black

-- This data type clarrifies the type of piece being moved by the player. This will affect the limitations of the piece.

data Type = Single | Double

-- This type tells us the position of the square on the board and what it holds.

type Square = (Int, Fill)

-- This type contains a move. The first integer in the tuple is the location of the 
-- checker and the second integer is the location the player wants to move their piece.

type Move = (Int,Int)

-- This type represnts our board.

type Board = [Square]


--                                                      Functions

-- Checks if a move, based on the type and color of the piece, is legal.

isValidMove :: Move -> Color -> Bool
isValidMove = undefined

-- Once a move is confirmed to be legal, we can update the board to reflect the new Move.

updateBoard :: Board -> Move -> Board
updateBoard = undefined


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




