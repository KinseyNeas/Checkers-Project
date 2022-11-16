module Solver where
-- ( whoWins
-- , bestMove
-- )
import Checkers

whoWillWin :: GameState -> Outcome
whoWillWin gs@(c, board, mLoc, ct) =
    case gameStatus gs of
        Nothing -> whoWillWin newGS
        Just RedWin -> RedWin
        Just BlackWin -> BlackWin
    where Just newGS = makeMove gs (bestMove gs)
    --ask about unsafe pattern matching (Just newGS)

bestMove :: GameState -> Move
bestMove = undefined --validMoves