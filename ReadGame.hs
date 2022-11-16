module ReadGame where
import Checkers
import Data.List.Split
--playCheckers :: GameState -> IO()
--playCheckers game = do
--    if not (checkGameOver game)
--       then continueGame
--    else endGame where
--    continueGame = undefined
--    endGame = undefined

data Outcome = RedWin | BlackWin | Tie

readGame :: String -> Maybe GameState -- converts file String to GameState
readGame a = let (x:xs) = splitOn "| " a
                  col = zip [0..7] splitOn "\n" x
                  board = makeBoard splitOn "  " col
             in (color, board, maybeLoc, count)

makeBoard :: String -> Maybe Board
makeBoard (x:xs) = 
makeBoard [] = Nothing

--     case zip [0 .. 7] (splitOn "," shroom) of
--        (x:xs) -> sequence (catMaybes ((map observationWord xs)), observationNom x) -- Return the Observation... maybe
 --       [] -> Nothing

showGame :: GameState  -> String -- converts GameState to file String
showGame = undefined

writeGame :: GameState -> FilePath -> IO () -- showGame() to IO() [Writes the game]
writeGame = undefined

loadGame :: FilePath -> IO GameState -- readGame() to get the gamestate and show in IO
loadGame = undefined

putWinner :: Game -> IO () -- Hold on this one for the other group
putWinner = undefined

