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
    | Win col `elem` lst = Win col
    | Tie `elem` lst = Tie
    | otherwise = opponent
    where opponent = case col of
                    Red -> Win Black
                    Black -> Win Red

bestMove :: GameState -> Move
bestMove gs@(c, board, mLoc, ct) = case gameStatus gs of 
            Nothing -> let validMovesLst = validMoves gs
                           --gsLst = catMaybes $ map (makeMove gs) validMovesLst
                           input = catMaybes [pullMaybe (a, makeMove gs a) | a <- validMovesLst]     --zip validMovesLst gsLst
                           bestOutcomeLst = map (\(x,y) -> bestMoveHelp x y) input
                           (mv, otc) = getTuple bestOutcomeLst c
                        in mv
            winner -> error "Hey! The game's over already!"

pullMaybe :: (a, Maybe b) -> Maybe (a,b)
pullMaybe (a,b) = case b of
                Just b -> Just (a,b)
                Nothing -> Nothing 
                           
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
                

-- Int represents depth
goodMove :: GameState -> Move -> Int
goodMove = undefined

whoMightWin :: GameState -> Int
whoMightWin gs@(c, board, mLoc, ct) = case gameStatus gs of
                        Just (Win col) -> if col == c then 100 else -100
                        Just Tie -> 0
                        Nothing -> scoreBoard gs 

scoreBoard :: GameState -> Int
scoreBoard gs@(c, board, mLoc, ct) = p1 - p2
        where p1 = scorePlayer board c
              p2 = scorePlayer board (enemy c)

scorePlayer :: Board -> Color -> Int
scorePlayer [] col = 0
scorePlayer (x:xs) col
                | c == col && roy == King && edgePiece loc = 11 + scorePlayer xs col
                | c == col && roy == King = 10 + scorePlayer xs col
                | c == col && edgePiece loc = 6 + scorePlayer xs col
                | c == col = 5 + scorePlayer xs col
                | otherwise = scorePlayer xs col
                where (loc,(c,roy)) = x

edgePiece :: Loc -> Bool
edgePiece (x,y) = x == 0 || x == 7 

enemy :: Color -> Color
enemy c = case c of
            Red -> Black
            Black -> Red