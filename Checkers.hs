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

-- This type contains a move. The first integer in the tuple is the location of the 
-- checker and the second integer is the location the player wants to move their piece.

type Loc = (Int, Int)

type Move = (Loc,Loc)

-- This type represnts our board.

type Board = [(Loc, Piece)]

type GameState = (Color, Board, Maybe Loc)

redKingedLocations :: [Loc]
redKingedLocations = [(1,0),(3,0),(5,0),(7,0)]

blackKingedLocations :: [Loc]
blackKingedLocations = [(0,7),(2,7),(4,7),(6,7)]


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
isValidMove :: GameState -> Move -> Bool
isValidMove (turn, board, mLoc) (loc1,loc2)
    | (moveInBounds (loc1,loc2)) && (lookUpPiece loc2 board) == Nothing =
                case Just (col, roy) of
                    Nothing -> False
                    Just (turn, noKing) -> captureMove || (isValidnoKingMove (col, roy) (loc1,loc2) (turn, board, mLoc))
                    Just (turn, King) -> captureMove || (isValidKingMove (loc1,loc2) (turn, board, mLoc))
                    Just (_,_) -> False
    | otherwise = False
    where Just (col, roy) = lookUpPiece loc1 board
          captureMove = isCapturedPiece (col, roy) (loc1,loc2) (turn, board, mLoc)

-- Also need to check that validmove because made by player
-- Haven't incoporated mLoc yet

-- Determines if a move is a capture move.
isCapturedPiece :: Piece -> Move -> GameState -> Bool
isCapturedPiece (col, roy) ((x1,y1),(x2,y2)) (turn, board, mLoc)
        | Just loc3 == Nothing = False
        | otherwise =
            case (col, roy) of
                (turn, King) -> kingCapture && hopingOther
                (turn, noKing) -> nokingCapture && hopingOther
                (_, _) -> False
        where Just loc3 = capturedPieceLoc ((x1,y1),(x2,y2))
              Just (capCol, capRoy) = lookUpPiece loc3 board
              hopingOther = if turn == Red then capCol == Black else capCol == Red 
              kingCapture = abs(y2 - y1) == 2 && abs(x2 - x1) == 2
              nokingCapture
                    | col == Red = abs(x2 - x1) == 2 && (y1 - 2 == y2)
                    | col == Black = abs(x2 - x1) == 2 && (y1 + 2 == y2)

-- If a move is suspected to be a capture, this returns the location of the piece that would be captured.
capturedPieceLoc :: Move -> Maybe Loc
capturedPieceLoc ((x1,y1),(x2,y2)) = 
    let (xdir, ydir) = (x1 - x2, y1 - y2)
    in case (xdir, ydir) of
            (-2, -2) -> Just (x1 + 1, y1 + 1)
            (-2, 2) -> Just (x1 + 1, y1 - 1)
            (2, -2) -> Just (x1 - 1, y1 + 1)
            (2, 2) -> Just (x1 - 1, y1 - 1)
            (_,_) -> Nothing
-- if the piece is a King it can caputure a piece backwards, if not only forwards, need to check that the 
-- spot trying to be captured is occupied by a piece with opposit color of person turn, and that hope is in correct direction, need to make sure place moving to is empty?

-- This function determines if the move being made is Vaild if the piece being moved is a King.
isValidKingMove :: Move -> GameState -> Bool
isValidKingMove ((x1,y1),(x2,y2)) (turn, board, mLoc) = (y2 == y1 - 1 || y2 == y1 + 1) && (x2 == x1 + 1 || x2 == x1 - 1)

-- This function determines if the move being made is Vaild if the piece being moved is NOT a King.
isValidnoKingMove :: Piece -> Move -> GameState -> Bool
isValidnoKingMove (color, royal) ((x1,y1),(x2,y2)) (turn, board, mLoc) =
                case color of
                    Red -> (y2 == y1 - 1) && (x2 == x1 + 1 || x2 == x1 - 1)
                    Black -> (y2 == y1 + 1) && (x2 == x1 + 1 || x2 == x1 - 1)

-- This checks that a move is made within the bounds of the board.
moveInBounds :: Move -> Bool
moveInBounds ((x1,y1),(x2,y2)) = not (x2 > 7 || x2 < 0 || y2 > 7 || y2 < 0)

-- This check if piece exists in location and if so, returns a Maybe Piece.
lookUpPiece :: Loc -> Board -> Maybe Piece
lookUpPiece loc board = lookup loc board

--                                ^ Above is functionality for isValidMove ^


-- Need to add implimentation for if King

validMoves :: GameState -> [Move] 
validMoves (c, board, mLoc) = 
    if (mLoc == Nothing) then
        colorValidMoves (c, board, mLoc) -- Just makes valid moves list
    else
        doubleJumpValidMoves (c, board, mLoc) -- Takes into account if we have a maybeLoc

--justValidMoves :: GameState -> [Move]
--justValidMoves (c, board, mLoc) =  
--    let Just (mx,my) = mLoc
--        piece = foldr (\(l,p) (a,c) -> if(a == l) then (l,p) else (a,c)) ((mx,my), (c, NoKing)) board
--        (xl2,xr2,y2) = if (fst(snd piece) == Red) then (mx-1,mx+1,my-1) else (mx+1,mx-1,my+1)
--        idL = [(l,p)|(l,p) <- board, (xl2,y2) == l]
--        idR = [(l,p)|(l,p) <- board, (xr2,y2) == l]
--        l3L = if (fst(snd piece) == Red) then (mx-2,my-2) else (mx+2,my+2)
--        l3R = if (fst(snd piece) == Red) then (mx+2,my-2) else (mx-2,my+2)
--    in  if (isValidMove ((mx,my),(xl2,y2)) (c, board, mLoc) == True && isValidMove ((mx,my),(xr2,y2)) (c, board, mLoc) == True) then  [((mx,my),(xl2,y2)),((mx,my),(xr2,y2))]
--        else if (isValidMove ((mx,my),(xl2,y2)) (c, board, mLoc) == True && isCapture ((mx,my),(xl2,y2)) (c, board, mLoc) idR == True) then  [((mx,my),(xl2,y2)),((mx,my),l3R)]
--        else if (isCapture ((mx,my),(xr2,y2)) (c, board, mLoc) idL == True && isValidMove ((mx,my),(xr2,y2)) (c, board, mLoc) == True) then  [((mx,my),l3L),((mx,my),(xr2,y2))]
--        else if (isCapture ((mx,my),(xl2,y2)) (c, board, mLoc) idL == True && isCapture ((mx,my),(xr2,y2)) (c, board, mLoc) idR == True) then  [((mx,my),l3L),((mx,my),l3R)]
--        else if (isValidMove ((mx,my),(xl2,y2)) (c, board, mLoc) == True) then  [((mx,my),(xl2,y2))]
--        else if (isValidMove ((mx,my),(xr2,y2)) (c, board, mLoc) == True) then  [((mx,my),(xr2,y2))]
--        else if (isCapture ((mx,my),(xr2,y2)) (c, board, mLoc) idL == True) then  [((mx,my),l3L)]
--        else if (isCapture ((mx,my),(xl2,y2)) (c, board, mLoc) idR == True) then  [((mx,my),l3R)]
--        else []

doubleJumpValidMoves :: GameState -> [Move]
doubleJumpValidMoves (c, board, Just (x,y)) = 
    case Just (col, roy) of
        Nothing -> []
        Just (c, roy) -> filter (isValidMove (c, board, Just (x,y))) unfilteredMoveLst
    where Just (col, roy) = lookUpPiece (x,y) board
          unfilteredMoveLst = [((x,y),(x-2,y-2)), ((x,y),(x-2,y+2)), ((x,y),(x+2,y-2)), ((x,y),(x+2,y+2))]

colorValidMoves :: GameState -> [Move]
colorValidMoves (c, board, mLoc) = undefined-- foldr (\((x,y),p) acc -> 
--    let (xl2,xr2,y2) = if (c == Red) then (x-1,x+1,y-1) else (x+1,x-1,y+1) -- Puts restraints on where a noKing piece can move
--        idL = [(l,p)|(l,p) <- board, (xl2,y2) == l] -- pieces located in left diagonal to call isCapture on
--        idR = [(l,p)|(l,p) <- board, (xr2,y2) == l] -- pieces located in right diagonal to call isCapture on
--        l3L = if (c == Red) then (x-2,y-2) else (x+2,y+2)
--        l3R = if (c == Red) then (x+2,y-2) else (x-2,y+2)
--    in  if (isValidMove ((x,y),(xl2,y2)) (c, board, mLoc) == True && isValidMove ((x,y),(xr2,y2)) (c, board, mLoc) == True && fst p == c) then acc ++ [((x,y),(xl2,y2))] ++ [((x,y),(xr2,y2))]
--        else if (isValidMove ((x,y),(xl2,y2)) (c, board, mLoc) == True && isCapture ((x,y),(xl2,y2)) (c, board, mLoc) idR == True && fst p == c) then acc ++ [((x,y),(xl2,y2))] ++ [((x,y),l3R)]
--        else if (isCapture ((x,y),(xr2,y2)) (c, board, mLoc) idL == True && isValidMove ((x,y),(xr2,y2)) (c, board, mLoc) == True && fst p == c) then acc ++ [((x,y),l3L)] ++ [((x,y),(xr2,y2))]
--        else if (isCapture ((x,y),(xl2,y2)) (c, board, mLoc) idL == True && isCapture ((x,y),(xr2,y2)) (c, board, mLoc) idR == True && fst p == c) then acc ++ [((x,y),l3L)] ++ [((x,y),l3R)]
--        else if (isValidMove ((x,y),(xl2,y2)) (c, board, mLoc) == True && fst p == c) then acc ++ [((x,y),(xl2,y2))]
--        else if (isValidMove ((x,y),(xr2,y2)) (c, board, mLoc) == True && fst p == c) then acc ++ [((x,y),(xr2,y2))]
--        else if (isCapture ((x,y),(xr2,y2)) (c, board, mLoc) idL == True && fst p == c) then acc ++ [((x,y),l3L)]
--        else if (isCapture ((x,y),(xl2,y2)) (c, board, mLoc) idR == True && fst p == c) then acc ++ [((x,y),l3R)]
--        else acc) [] board

isCapture :: Move -> GameState -> [(Loc, Piece)] -> Bool
isCapture (l1,l2) (c, board, mLoc) [] = False
isCapture (l1,l2) (c, board, mLoc) [(m,p2)] = 
    let l3 = (if( fst l2 - fst l1 > 0) then fst l2 + 1 else fst l2 - 1, if( snd l2 - snd l1 > 0) then snd l2 + 1 else snd l2 - 1)
        id3 = [(l,p)|(l,p) <- board, l3 == l]
    in  if (fst p2 /= c) then
            if (id3 == [] && fst l3 <= 7 && fst l3 > 0 && snd l3 <= 7 && snd l3 > 0) then True
            else False
        else False

-- Also need to check that validmove because made by player
-- Haven't incoporated mLoc yet

-- Once a move is confirmed to be legal, we can update the board to reflect the new Move.

makeMove :: GameState -> Move -> Maybe GameState
makeMove gState move = 
    let x = isValidMove gState move
    in case x of
            False -> Nothing
            True -> Just (updateState gState move)

updateState :: GameState -> Move -> GameState
updateState (c, board, mLoc) move = (nextPlayer, updateBoard board move, mLoc)
    where nextPlayer = if c == Red then Black else Red

updateBoard :: Board -> Move -> Board
updateBoard board (l1, l2) = [if loc == l1 then (l2, updateClass piece l2) else (loc, piece) | (loc, piece) <- board]

updateClass :: Piece -> Loc -> Piece
updateClass (color, currClass) loc = if makeKing then (color, King) else (color, currClass)
    where makeKing = if color == Red then loc `elem` redKingedLocations else loc `elem` blackKingedLocations

--Once a move is made, we will need to check whether or not that piece needs to be kinged or not.
-- Function used to check if the game is over.

checkGameOver :: GameState -> Bool
checkGameOver (c, board, mLoc) = r == 0 || b == 0
    where (r,b) = foldr (\(loc, (color, king)) (r,b) -> if color == Red then (r+1,b) else (r,b+1)) (0,0) board

checkWinner :: GameState -> Color
checkWinner (c, ((x,y):ys), mLoc) = fst y

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




