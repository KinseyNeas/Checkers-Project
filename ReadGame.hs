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

readGame :: String -> GameState -- converts file String to GameState
readGame a = let (x:xs) = splitOn " | " a
                row = zip [0..7] (splitOn "\n" x)
                board = concat[makeBoard (splitOn "  " text) x y | (y,text) <- row]
                color = findColor xs
                maybeLoc = findMaybeLoc xs
                count = findCount xs
             in (color, board, maybeLoc, count)

makeBoard :: [String] -> Int -> Int -> Board
makeBoard [] x y = []
makeBoard (z:zs) x y = case z of
    "__" -> makeBoard zs x+1 y
    "RK" -> (((x,y),(Red,King)):makeBoard zs x+1 y)
    "R_" -> (((x,y),(Red,NoKing)):makeBoard zs x+1 y)
    "BK" -> (((x,y),(Black,King)):makeBoard zs x+1 y)
    "B_" -> (((x,y),(Black,NoKing)):makeBoard zs x+1 y)

findColor :: [String] -> Color
findColor (x:xs) = case x of
                        "Black's turn" -> Black
                        "Red's turn" -> Red

maybeLoc :: [String] -> Maybe Loc
maybeLoc [z,zz,zs] = 
    let broken = splitOn " " zs
        c = findNum broken
        (xf,yf) = splitOn "," c
        (x,y) = (concat[b | b<-xf, b /= '(', b /= ')'],concat[b | b<-yf, b /= '(', b /= ')'])
    in Just (read x :: Int, read y :: Int)             
maybeLoc [z,zz] = Nothing

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
--     case zip [0 .. 7] (splitOn "," shroom) of
--        (x:xs) -> sequence (catMaybes ((map observationWord xs)), observationNom x) -- Return the Observation... maybe
 --       [] -> Nothing

showGame :: GameState  -> String -- converts GameState to file String
showGame g = printBoard g

writeGame :: GameState -> FilePath -> IO () -- showGame() to IO() [Writes the game]
writeGame g fp = do
                    txt <- showGame g
                    writeFile fp 

loadGame :: FilePath -> IO GameState -- readGame() to get the gamestate and show in IO
loadGame fp = do 
                gs <- readGame (readFile fp)
                putStrLn gs

putWinner :: Game -> IO () -- Hold on this one for the other group
putWinner = undefined

