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
      moveIO (inputMove flags gs) gs
      (chooseAction flags) gs

chooseAction :: [Flag] -> GameState -> IO()
chooseAction flags gs 
   | Winner `elem` flags = findWinner flags gs
   | otherwise = emptyDefault flags gs

findWinner :: [Flag] -> GameState -> IO()
findWinner flags gs = do putStrLn $ show $ bestMove gs --showGame gs 

setDepth :: [Flag] -> Int
setDepth ((Depth x):_) = read x :: Int
setDepth (_:flags)  = setDepth flags 
setDepth []  = 4

inputMove :: [Flag] -> GameState -> Maybe Move
inputMove ((Move a):_) gs = 
   let xf = splitOn "," a
       [x1,y1,x2,y2] = [b | b<-xf, b /= "(", b /= ")", b /= " "]
   in Just ((read x1 :: Int, read y1 :: Int),(read x2 :: Int, read y2 :: Int))
inputMove (_:flags) gs = inputMove flags gs
inputMove [] gs = Nothing

moveIO :: Maybe Move -> GameState -> IO()
moveIO (Just x) gs = do 
   let ng = updateState gs x
   putStrLn $ showGame ng
moveIO Nothing gs = do putStrLn ""

beVerbose :: [Flag] -> GameState -> Move -> IO()
beVerbose flags gs m =
   if Verbose `elem` flags
      then do
         putStrLn $ show m
         putStrLn $ wintoStr $ whoWillWin $ updateState gs m
      else putStrLn ""

emptyDefault :: [Flag] -> GameState -> IO()
emptyDefault flags gs = do 
   let d = setDepth flags
   if Verbose `elem` flags
      then do 
         let mv = goodMove gs d
         beVerbose flags gs mv
         putStrLn $ show $ mv
   else do putStrLn $ show $ goodMove gs d
   