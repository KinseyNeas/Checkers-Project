module Solver where
import Data.Maybe
-- ( whoWins
-- , bestMove
-- )
import Checkers

--whoWillWin :: GameState -> Outcome
--whoWillWin gs@(c, board, mLoc, ct) =
--    case gameStatus gs of
--        Just x -> x
--        Nothing -> whoWillWin newGS
--    where Just newGS = makeMove gs (bestMove gs)
    --ask about unsafe pattern matching (Just newGS)
--data Tree = Node [Tree] | Leaf Outcome deriving Show

whoWillWin :: GameState -> Outcome
whoWillWin gs@(c, board, mLoc, ct) = 
    case gameStatus gs of
        Just x -> x
        Nothing -> let vMoves = validMoves gs
                       gsLst = catMaybes $ map (makeMove gs) vMoves
                       outcomeLst = map whoWillWin gsLst
                    in decideWinner outcomeLst c 

decideWinner :: [Outcome] -> Color -> Outcome
decideWinner lst col  
    | currPlayer `elem` lst = currPlayer
    | Tie `elem` lst = Tie
    | otherwise = opponent
    where currPlayer = case col of 
                    Red -> Win Red
                    Black -> Win Black
          opponent = case currPlayer of
                    Win Red -> Win Black
                    Win Black -> Win Red

-- psuedocode

-- gameStatus gs
-- validMoves gs -> list of valid moves
-- for each validMove, call makeMove -> list of game states
-- needs to go through each possible situation until it finds a match

-- Fogarty likes 

bestMove :: GameState -> Move
bestMove = undefined --validMoves