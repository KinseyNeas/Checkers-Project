module Checkers where
import Data.List (sort)
import Debug.Trace
import Data.List

--                                                      Type Alises
--
-- This data type is meant to represent what a tile on our board contains at any given moment.
--
data Fill = Empty | Checker Color Type
--
-- This data type differentiates the color of the pieces so that we can keep track of which
-- player is playing/check if they are moving the correct pieces.
--
data Color = Red | Black
--
-- This data type clarrifies the type of piece being moved by the player. This will affect the limitations of the piece.
--
data Type = Single | Double
--
-- This type tells us the position of the square on the board and it holds.
--
type Square = (Int, Fill)
--
-- This type contains a move. The first integer in the tuple is the location of the 
-- checker and the second integer is the location the player wants to move their piece.
--
type Move = (Int,Int)
--
-- This type represnts our board.
--
type Board = [Square]
--
--
--                                                      Functions
--
-- Checks if a move, based on the type and color of the piece, is legal.
--
isValidMove :: Move -> Color -> Bool
isValidMove = undefined
--
-- Once a move is confirmed to be legal, we can update the board to reflect the new Move.
--
updateBoard :: Board -> Move -> Board
updateBoard = undefined
--
--
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




