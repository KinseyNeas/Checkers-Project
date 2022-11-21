module Checkers where
import Data.List (sort)
import Debug.Trace
import Data.List
import Data.Maybe
--import Data.catMaybe

--                                                      Type Aliases

-- This data type is meant to represent what a tile on our board contains at any given moment.

type Piece = (Color, Class)

-- This data type differentiates the color of the pieces so that we can keep track of which
-- player is playing/check if they are moving the correct pieces.

data Color = Red | Black deriving (Show, Eq)

-- This data type clarifies the type of piece being moved by the player. This will affect the limitations of the piece.

data Class = NoKing | King deriving (Show, Eq)

data Outcome = Win Color | Tie deriving (Show, Eq)

-- This type contains a move. The first integer in the tuple is the location of the 
-- checker and the second integer is the location the player wants to move their piece.

type Loc = (Int, Int) 

type Move = (Loc,Loc) 

-- This type represnts our board.

type Board = [(Loc, Piece)] 

type Count = Int    --Count is the game's turn counter. 

type GameState = (Color, Board, Maybe Loc, Count) 



shouldKingify :: Color -> Loc -> Bool
shouldKingify Red loc = loc `elem` [(1,0),(3,0),(5,0),(7,0)] 
shouldKingify Black loc = loc `elem` [(0,7),(2,7),(4,7),(6,7)]


--                                                     Print Function

-- Instances of show so that player can see board and pieces.

showPiece :: Piece -> String
showPiece (Red,King) = " RK "
showPiece (Red,NoKing) = " R_ "
showPiece (Black,King) = " BR "
showPiece (Black,NoKing) = " B_ "

printBoard :: GameState -> String --done in individual cells
printBoard (c,board,Nothing,ct) = intercalate "\n" [concat [printCell board (x,y)|x <- [0..7]] |y <- [0..7]] ++ " | " ++ show c ++ "'s turn | number of turns left: " ++ show ct
printBoard (c,board,Just l,ct) = intercalate "\n" [concat [printCell board (x,y)|x <- [0..7]] |y <- [0..7]] ++ " | " ++ show c ++ "'s turn | number of turns left: " ++ show ct ++ " | previously moved piece: " ++ show l

printCell :: Board -> Loc -> String
printCell [] _ = " __ "
printCell bLst l = 
    case lookup l bLst of
        Just loc -> showPiece loc
        Nothing -> " __ "
--                                                      Functions

-- Checks if a move is legal.
isValidMove :: GameState -> Move -> Bool
isValidMove gs@(turn, board, Nothing, ct) move@(loc1,loc2)
    | not (moveInBounds move) 
    || not (lookup loc2 board == Nothing) 
    || not (colorOfLoc board loc1 == Just turn) = False
    | otherwise = isCapturedPiece move gs || isValidDirection move gs

isValidMove gs@(turn, board, mLoc, ct) move@(loc1,loc2)
    | not (moveInBounds move) 
    || not (lookup loc2 board == Nothing) 
    || not (colorOfLoc board loc1 == Just turn) 
    || not (mLoc == Just loc1) = False
    | otherwise = isCapturedPiece move gs


--                                            Helper Functions for isValidMove 

colorOfLoc :: Board -> Loc -> Maybe Color
colorOfLoc board loc = 
        case lookup loc board of
            Nothing -> Nothing
            Just (c,roy) -> Just c

isCapturedPiece :: Move -> GameState -> Bool
isCapturedPiece move@((x1,y1),(x2,y2)) gs@(turn, board, mLoc, ct)
        | not $ isCapture move = False
        | otherwise = case (lookup (x1,y1) board, lookup loc3 board) of -- tuple of piece @ loc1 and piece @ loc3
                           (Nothing, _) -> False
                           (_, Nothing) -> False
                           (Just (color1, King), Just (color3, _)) -> color1 /= color3  && color1 == turn 
                           (Just (color1, NoKing), Just (color3, _)) -> color1 /= color3 && color1 == turn && nokingCapture turn
        where loc3 = capturedPieceLoc move
              nokingCapture :: Color -> Bool
              nokingCapture col
                    | col == Red = (y1 - 2 == y2)
                    | col == Black = (y1 + 2 == y2)

-- Basic test to determine if a move is a capture move or not.
isCapture :: Move -> Bool
isCapture ((x1,y1),(x2,y2)) = (abs $ y2 - y1) == 2 && (abs $ x2 - x1) == 2

-- If a move is suspected to be a capture, this returns the location of the piece that would be captured.
capturedPieceLoc :: Move -> Loc
capturedPieceLoc ((x1,y1),(x2,y2)) = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)

-- This function determines if the move being made is vaild based on class and color.
isValidDirection :: Move -> GameState -> Bool
isValidDirection ((x1,y1),(x2,y2)) (turn, board, mLoc, ct) =
                case lookup (x1,y1) board of
                    Just (_, King) -> (y2 == y1 - 1 || y2 == y1 + 1) && (x2 == x1 + 1 || x2 == x1 - 1)
                    Just (Red, NoKing) -> (y2 == y1 - 1) && (x2 == x1 + 1 || x2 == x1 - 1)
                    Just (Black, NoKing) -> (y2 == y1 + 1) && (x2 == x1 + 1 || x2 == x1 - 1)

-- This checks that a move is made within the bounds of the board.
moveInBounds :: Move -> Bool
moveInBounds ((x1,y1),(x2,y2)) = not (x2 > 7 || x2 < 0 || y2 > 7 || y2 < 0)


--                                ^ Above is functionality for isValidMove ^


-- Determines all the valid moves for a game state
validMoves :: GameState -> [Move] 
validMoves gs@(c, board, mLoc, ct) = 
                case mLoc of
                    Nothing -> colorValidMoves gs -- Just makes valid moves list
                    Just loc -> doubleJumpValidMoves gs -- Takes into account if we have a maybeLoc

-- If mLoc exists, this determines all the moves that can be made by the piece located at mLoc.
doubleJumpValidMoves :: GameState -> [Move]
doubleJumpValidMoves gs@(c, board, Just (x,y), ct) = 
    case lookup (x,y) board of
            Nothing -> []
            Just (c, roy) -> filter (isValidMove gs) unfilteredMoveLst
    where unfilteredMoveLst = [((x,y),(x-2,y-2)), ((x,y),(x-2,y+2)), ((x,y),(x+2,y-2)), ((x,y),(x+2,y+2))]

-- Determines all of the valid moves that can be made the pieces in a specific gamestate.
colorValidMoves :: GameState -> [Move]
colorValidMoves gs@(c, board, mLoc, ct) = filter (isValidMove gs) allMovesLst
    where allMovesLst = concat [allPossibleMoves (l,(col,roy)) | (l,(col,roy)) <- board, col == c]

-- Create all possible moves that could be made by one piece.
allPossibleMoves :: (Loc, Piece) -> [Move]
allPossibleMoves ((x,y), (col,King)) = [((x,y),(x-1,y-1)),((x,y),(x+1,y-1)),((x,y),(x-1,y+1)),((x,y),(x+1,y+1)),
                                        ((x,y),(x-2,y-2)),((x,y),(x+2,y-2)),((x,y),(x-2,y+2)),((x,y),(x+2,y+2))]
allPossibleMoves ((x,y), (Red,NoKing)) = [((x,y),(x-1,y-1)),((x,y),(x+1,y-1)),((x,y),(x-2,y-2)),((x,y),(x+2,y-2))]
allPossibleMoves ((x,y), (Black,NoKing)) = [((x,y),(x-1,y+1)),((x,y),(x+1,y+1)),((x,y),(x-2,y+2)),((x,y),(x+2,y+2))]

-- makeMove checks that a move is valid and then returns the resulting GameState
makeMove :: GameState -> Move -> Maybe GameState
makeMove gState move = 
    let x = isValidMove gState move
    in case x of
            False -> Nothing
            True -> Just (updateState gState move)

-- Updates the state, and updates mLoc based on whether the move is a capture.
-- Double jumps are forced.
updateState :: GameState -> Move -> GameState
updateState gs@(c, board, mLoc, ct) move@(loc1,loc2) 
    | isCapturedPiece move gs = case canDoubleJump (c, updatedCapBoard, Just loc2, newCt) of
                                        Nothing -> (nextPlayer, updatedCapBoard, Nothing, newCt)
                                        loc -> (c, updatedCapBoard, loc, newCt)
    | otherwise = (nextPlayer, updatePiece board move, Nothing, newCt)
    where nextPlayer = if c == Red then Black else Red
          updatedCapBoard = updateCapturePiece board move
          newCt = ct - 1

canDoubleJump :: GameState -> Maybe Loc
canDoubleJump gs@(c, board, loc2, ct) = 
    case validMoves gs of
            [] -> Nothing
            lst -> loc2

updateCapturePiece :: Board -> Move -> Board
updateCapturePiece board move = removePiece (updatePiece board move) move

removePiece :: Board -> Move -> Board
removePiece board move = [(loc, piece) | (loc, piece) <- board, loc /= capturedPiece]
    where capturedPiece = capturedPieceLoc move


-- Updates the board assuming the move is valid.
-- Need to remove middle piece if move is capture... !!!
updatePiece :: Board -> Move -> Board
updatePiece board (l1, l2) = [if loc == l1 then (l2, updateClass piece l2) else (loc, piece) | (loc, piece) <- board]

-- Determines if a piece's royalty needs to be updated.
updateClass :: Piece -> Loc -> Piece
updateClass (color, currClass) loc
            | shouldKingify color loc = (color, King) 
            | otherwise = (color, currClass)
--Once a move is made, we will need to check whether or not that piece needs to be kinged or not.


-- Function used to check if the game is over.
checkGameOver :: GameState -> Bool
checkGameOver (c, board, mLoc, ct) = r == 0 || b == 0
    where (r,b) = foldr countFunc (0,0) board
          countFunc (loc, (Red, king)) (r,b) = (r+1,b)
          countFunc (loc, (Black, king)) (r,b) = (r,b+1)

-- Checks the wimmer assuming the game is over.
--data Outcome = RedWin | BlackWin | Tie

getWinner :: GameState -> Outcome
getWinner (c, ((x,y):ys), mLoc, ct) = 
    case fst y of 
        Red -> Win Red
        Black -> Win Black

gameStatus :: GameState -> Maybe Outcome
gameStatus gs@(c, board, mLoc, ct)
    | ct == 0 = Just Tie
    | checkGameOver gs = Just (getWinner gs)
    | otherwise = Nothing

--whoHasWon :: GameState -> Maybe Outcome
--whoHasWon gs@(c, board, mLoc, ct) = undefined

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




