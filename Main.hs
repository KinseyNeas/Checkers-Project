module Main where
import Checkers
import TesterCode
import Solver
import Data.Maybe
import System.IO
import Control.Monad
import System.Exit
import Text.Read
import System.Environment
import System.Console.GetOpt

data Flag = Winner | Depth String | Help | Move String | Verbose deriving (Ed,Show)

options :: [OptDescr Flag]
options = [Options ['w'] ["winner"] (noArg) "print out the best move"
         , Options ['d'] ["depth"] (RegArg Depth "<num>") "Use <num> as a cutoff depth"
         , Options ['h'] ["help"] (noArg) "print out help message and quit the program"
         , Options ['m'] ["move"] (RegArg Move "<move>") "make a <move> and print out the resulting board"
         , Options ['v'] ["verbose"] (noArg) "output the move and a description of how good it is"]

main :: IO ()
main = do
   args <- getArgs
   let (flags, inputs, error) = getOpt Permute options args
   putStrLn $ show (flags, inputs, error)
   if Help `elem` flags
      then putStrLn $ usageinfo "Checkers [options] [file]" options
   else do
      let fname = if null inputs then "game.txt" else head inputs
      gs <- loadGame fname 
      findWinner flags gs
      setDepth flags gs
      inputMove flags gs
      beVerbose flags gs --how does verbose work?
      emptyDefault flags gs

findWinner :: [Flag] -> GameState -> IO()
findWinner flags gs = do
   if Winner `elem` flags
      then putStrLn $ bestMove gs
   else putStrLn ""

setDepth :: [Flag] -> GameState -> IO()--change later
setDepth ((Depth x):_) (turn, board, Nothing, ct) = do -- figure out how to set the depth
   let d = read x :: Int
   let game = (turn, board, Nothing, d)
   putStrLn $ bestMove game -- is this right? or I could use writeFile?
setDepth (_:flags) gs = setDepth flags
setDepth [] gs = do putStrLn ""

inputMove :: [Flags] -> [GameState] -> IO()
inputMove ((Move a):_) gs = do
   let (xf:xfs) = splitOn "," a
   let x = [b | b<-xf, b /= "(", b /= ")", b /= " "]
   let y = [b | b<-xfs, b /= "(", b /= ")", b /= " "]
   let mv = (read x :: Int, read y :: Int)
   let newgs = updateState gs mv
   putStrLn $ showGame newgs
inputMove (_:flags) gs = inputMove flags
inputMove [] gs = do putStrLn "" 

beVerbose :: [Flag] -> GameState -> IO()
beVerbose flags gs = do
   if Verbose `elem` flags
      then 
            let m = bestMove gs
            putStrLn m
            putStrLn $ wintoStr $ whoWillWin m
   else putStrLn ""

emptyDefault :: [Flags] -> GameState -> IO()
emptyDefault [] (turn, board, Nothing, ct) = do -- figure out how to set the depth
   let game = (turn, board, Nothing, 4) --default depth is 4 for now
   putStrLn $ bestMove game
emptyDefault _ gs = do putStrLn ""
