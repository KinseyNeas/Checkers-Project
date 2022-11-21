module Solver where
import Data.Maybe
-- ( whoWins
-- , bestMove
-- )
import Checkers
import TesterCode


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

bestMove :: GameState -> Move
bestMove gs@(c, board, mLoc, ct) = case gameStatus gs of 
            Nothing -> let validMovesLst = validMoves gs
                           gsLst = catMaybes $ map (makeMove gs) validMovesLst
                           input = zip validMovesLst gsLst
                           bestOutcomeLst = map (\(x,y) -> bestMoveHelp x y) input
                           (mv, otc) = getTuple bestOutcomeLst c
                        in mv
            winner -> error "Hey! The game's over already!"
                           
bestMoveHelp :: Move -> GameState -> (Move, Outcome)
bestMoveHelp move gs@(c, board, mLoc, ct) = case gameStatus gs of 
                Nothing -> let validMovesLst2 = validMoves gs
                               newGsLst = catMaybes $ map (makeMove gs) validMovesLst2
                               outcomeLst = map (bestMoveHelp move) newGsLst
                            in getTuple outcomeLst c
                Just x -> (move, x)

getTuple :: [(Move, Outcome)] -> Color -> (Move, Outcome)
getTuple lst@(l:ls) c = case [(mv, otc) | (mv, otc) <- lst, otc == Win c] of
                        (x:xs) -> x
                        [] -> case [(mv, otc) | (mv, otc) <- lst, otc == Tie] of
                                (y:ys) -> y
                                [] -> l
                

