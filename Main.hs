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
      case inputMove flags gs of
         Nothing -> (chooseAction flags) gs
         Just mv -> do
                     moveIO flags mv gs
                     --(chooseAction flags) gs

chooseAction :: [Flag] -> GameState -> IO()
chooseAction flags gs 
   | Winner `elem` flags = findWinner flags gs
   | flags == [] = emptyDefault flags gs
   | otherwise = do
                     putStrLn "not a valid flag"
                     putStrLn $ usageInfo "Checkers [options] [file]" options

findWinner :: [Flag] -> GameState -> IO()
findWinner flags gs = case bestMove gs of
                           Nothing -> putStrLn "The game is already over! There's no move to be made."
                           Just mv -> if Verbose `elem` flags 
                                      then do putStrLn $ show mv
                                              beVerbose gs mv 
                                      else putStrLn $ show mv
   
  -- putStrLn $ show $ bestMove gs --showGame gs 

setDepth :: [Flag] -> Int
setDepth ((Depth x):_) = read x :: Int
setDepth (_:flags)  = setDepth flags 
setDepth [] = 4

inputMove :: [Flag] -> GameState -> Maybe Move
inputMove ((Move a):_) gs = Just (read a :: Move) 
inputMove (_:flags) gs = inputMove flags gs
inputMove [] gs = Nothing

moveIO :: [Flag] -> Move -> GameState -> IO()
moveIO flags move gs = do
    case makeMove gs move of
            Nothing -> do putStrLn $ showGame gs
                          putStrLn "This is an invalid move. Choose another one!"
            Just newGS ->  if Verbose `elem` flags 
                           then do putStrLn $ showGame newGS
                                   beVerbose gs move 
                           else putStrLn $ showGame newGS
                                          --putStrLn $ show mv
--moveIO flags Nothing gs = putStrLn "death"

beVerbose :: GameState -> Move -> IO()
beVerbose gs@(c, board, mLoc, count) m
            | 100 == score = putStrLn "This move leads to a win!"
            | 0 < score = putStrLn "This move gives you an advantage!" 
            | 0 == score = putStrLn "This move leads to an even board!"
            | -100 == score = putStrLn "This move leads to a loss!"
            | otherwise = putStrLn "This move puts you at a disadvantage!"
            where score = whoMightWin gs c--(updateState gs m) c

     

emptyDefault :: [Flag] -> GameState -> IO()
emptyDefault flags gs = case goodMove gs d of
   Nothing -> putStrLn "The game is over! No moves to make."
   Just mv ->  if Verbose `elem` flags
               then do 
                     beVerbose gs mv
                     putStrLn $ show mv
               else do putStrLn $ show mv
   where d = setDepth flags
   