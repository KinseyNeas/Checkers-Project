module ReadGame where
import Checkers
import Solver
import Data.List.Split
--playCheckers :: GameState -> IO()
--playCheckers game = do
--    if not (checkGameOver game)
--       then continueGame
--    else endGame where
--    continueGame = undefined
--    endGame = undefined

readGame :: String -> Maybe GameState -- converts file String to GameState
readGame a = case splitOn " | " a of
                [] ->  Nothing
                (x:xs) -> let row = zip [0..7] (splitOn " \\n " x)
                              board = concat[makeBoard (splitOn "  " text) 0 y | (y,text) <- row]
                              color = findColor xs
                              maybeLoc = findMaybeLoc xs
                              count = findCount xs
                          in Just (color, board, maybeLoc, count)

makeBoard :: [String] -> Int -> Int -> Board
makeBoard [] x y = []
makeBoard (z:zs) x y = case z of
    " __" -> makeBoard zs (x+1) y
    "__ " -> makeBoard zs (x+1) y
    "__" -> makeBoard zs (x+1) y
    "RK" -> (((x,y),(Red,King)):makeBoard zs (x+1) y)
    "R_" -> (((x,y),(Red,NoKing)):makeBoard zs (x+1) y)
    "BK" -> (((x,y),(Black,King)):makeBoard zs (x+1) y)
    "B_" -> (((x,y),(Black,NoKing)):makeBoard zs (x+1) y)
    _ -> makeBoard zs x y

findColor :: [String] -> Color
findColor (x:xs) = case x of
                        "Black's turn" -> Black
                        "Red's turn" -> Red

findMaybeLoc :: [String] -> Maybe Loc
findMaybeLoc [z,zz,zs] = 
    let broken = splitOn " " zs
        c = findNum broken
        xf = splitOn "," c
        [x,y] = [b | b<-xf, b /= "(", b /= ")"]
    in Just (read x :: Int, read y :: Int)             
findMaybeLoc [z,zz] = Nothing

findCount :: [String] -> Count
findCount [x,xx,xs] = 
    let broken = splitOn " " xx
        c = findNum broken
    in read c :: Int     
findCount [x,xx] = 
    let broken = splitOn " " xx
        c = findNum broken
    in read c :: Int

findNum :: [String] -> String
findNum (x:xx:[]) = xx
findNum (x:xs) = findNum xs

showGame :: GameState  -> String -- converts GameState to file String
showGame g = printBoard g

writeGame :: GameState -> FilePath -> IO () -- showGame() to IO() [Writes the game]
writeGame g fp = do
                    let txt = showGame g
                    writeFile txt fp 

loadGame :: FilePath -> IO GameState -- readGame() to get the gamestate and show in IO
loadGame fp = do 
                fil <- readFile fp
                case readGame fil of
                     Nothing -> error "Error! There's no game to load."
                     Just game -> return game

putWinner :: GameState -> IO () -- Hold on this one for the other group
putWinner gs = do  
                 let win = whoWillWin gs
                 let strw = wintoStr win
                 putStrLn strw

wintoStr :: Outcome -> String
wintoStr o = case o of
    Win Red -> "Red Wins!"
    Win Black -> "Black Wins!"
    Tie -> "Its a tie!"
