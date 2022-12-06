module Main where
import Checkers
import TesterCode
import Solver
import ReadGame
import Data.Maybe
import System.IO
import Control.Monad
import System.Exit
import Text.Read
import System.Environment
import System.Console.GetOpt
import Data.List.Split


data Flag = Winner | Depth String | Help | Move String | Verbose deriving (Eq,Show)

options :: [OptDescr Flag]
options = [Option ['w'] ["winner"] (NoArg Winner) "print out the best move"
         , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Use <num> as a cutoff depth"
         , Option ['h'] ["help"] (NoArg Help) "print out help message and quit the program"
         , Option ['m'] ["move"] (ReqArg Move "<move>") "make a <move> and print out the resulting board"
         , Option ['v'] ["verbose"] (NoArg Verbose) "output the move and a description of how good it is"]

main :: IO ()
main = do
   args <- getArgs
   let (flags, inputs, error) = getOpt Permute options args
   --putStrLn $ show (flags, inputs, error)
   if Help `elem` flags
      then putStrLn $ usageInfo "Checkers [options] [file]" options
   else do
      let fname = if null inputs then "testGame.txt" else head inputs
      gs <- loadGame fname
      moveIO flags (inputMove flags gs) gs
      (chooseAction flags) gs

chooseAction :: [Flag] -> GameState -> IO()
chooseAction flags gs 
   | Winner `elem` flags = findWinner flags gs
   | otherwise = emptyDefault flags gs

findWinner :: [Flag] -> GameState -> IO()
findWinner flags gs = if Verbose `elem` flags 
                        then do putStrLn $ show mv
                                beVerbose gs mv 
                                else do putStrLn $ show mv
                     where mv = bestMove gs
   
  -- putStrLn $ show $ bestMove gs --showGame gs 

setDepth :: [Flag] -> Int
setDepth ((Depth x):_) = read x :: Int
setDepth (_:flags)  = setDepth flags 
setDepth [] = 4

inputMove :: [Flag] -> GameState -> Maybe Move
inputMove ((Move a):_) gs = Just (read a :: Move) 
inputMove (_:flags) gs = inputMove flags gs
inputMove [] gs = Nothing

moveIO :: [Flag] -> Maybe Move -> GameState -> IO()
moveIO flags (Just mv) gs = do 
   case makeMove gs mv of
         Nothing -> do putStrLn $ showGame gs
                       putStrLn "This is an invalid move. Choose another one!"
         Just newGS ->  if Verbose `elem` flags 
                        then do putStrLn $ showGame newGS
                                beVerbose gs mv 
                                else do putStrLn $ showGame newGS
                                        putStrLn $ show mv
moveIO flags Nothing gs = putStrLn ""

beVerbose :: GameState -> Move -> IO()
beVerbose gs@(c, board, mLoc, count) m
            | 100 == score = putStrLn "This move leads to a win!"
            | 0 == score = putStrLn "This move leads to a stalemate!"
            | -100 == score = putStrLn "This move leads to a loss!"
            | otherwise = putStrLn $ show score
            where score = whoMightWin gs c--(updateState gs m) c

     

emptyDefault :: [Flag] -> GameState -> IO()
emptyDefault flags gs = do 
   if Verbose `elem` flags
      then do 
         beVerbose gs mv
         putStrLn $ show mv
   else do putStrLn $ show mv
   where mv = goodMove gs d
         d = setDepth flags
   