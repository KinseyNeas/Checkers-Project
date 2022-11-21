module Solver where
import Data.Maybe
-- ( whoWins
-- , bestMove
-- )
import Checkers
import TesterCode

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
                

--bestMoveHelp move gs = case gameStatus gs of
--             Nothing -> (move, outcome)
--             Just x = let validMoveLst = validMoves gs
--                          newGsLst = map --(\x -> (move, makeMove x gs))-- validMoveLst -- just map make move
--                          outcomeLst = map (\(x,y) -> (x, bestMoveHelp x y))  - just call best help on all
--                        in deriveOutcome&Move outcomeLst

-- deriveOutcome&Move :: [(move, outcome)] -> Color -> (move,outcome)
-- deriveOutcome&Move lst@(x:xs) col = case [(x,y)|(x,y)<- lst, y == Win col] OR filter (\(x,y) -> y /= Win !Color ) lst of
--    (y:ys) -> y
--    [] -> case [(x,y)|(x,y)<- lst, y == Tie] of
--                (f:fs) -> f
--                [] -> x

--checkTie :: [()] -> Bool
--checkPlayer :: Color -> [()] -> Bool
--checkPlayer col lst = foldr (\(x,y) retVal -> y == Win col || )

-- opponentColor :: Color -> Color