module Solver
-- ( whoWins
-- , bestMove
-- )
where
import Checkers

data Outcome = RedWin | BlackWin | Tie

whoWillWin :: GameState -> Outcome
whoWillWin gs@(c, board, mLoc, ct) = 

bestMove :: GameState -> Move
bestMove 

