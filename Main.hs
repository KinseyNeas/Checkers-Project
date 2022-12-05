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
import Data.List.Split


data Flag = Winner | Depth String | Help | Move String | Verbose deriving (Eq,Show)

options :: [OptDescr Flag]
options = [Option ['w'] ["winner"] (NoArg) "print out the best move"
         , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Use <num> as a cutoff depth"
         , Option ['h'] ["help"] (NoArg) "print out help message and quit the program"
         , Option ['m'] ["move"] (ReqArg Move "<move>") "make a <move> and print out the resulting board"
         , Option ['v'] ["verbose"] (NoArg) "output the move and a description of how good it is"]

main :: IO ()
main = do
   args <- getArgs
   let (flags, inputs, error) = getOpt Permute options args
   putStrLn $ show (flags, inputs, error)
   if Help `elem` flags
      then putStrLn $ usageInfo "Checkers [options] [file]" options
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
      then putStrLn $ show $ bestMove gs
   else putStrLn ""

setDepth :: [Flag] -> GameState -> IO()
setDepth ((Depth x):_) gs = do 
   let d = read x :: Int
   putStrLn $ show $ goodMove gs d -- is this right? or I could use writeFile?
setDepth (_:flags) gs = setDepth flags gs
setDepth [] gs = do putStrLn ""

inputMove :: [Flag] -> GameState -> IO()
inputMove ((Move a):_) gs = do
   let xf = splitOn "," a
   let [x,y] = [b | b<-xf, b /= "(", b /= ")", b /= " "]
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
         in putStrLn $ show m
            putStrLn $ wintoStr $ whoWillWin m
   else putStrLn ""

emptyDefault :: [Flag] -> GameState -> IO()
emptyDefault [] gs = do putStrLn $ show $ goodMove gs 4    --default depth is 4 for now
emptyDefault _ gs = do putStrLn ""
