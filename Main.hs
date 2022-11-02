module Main where
import Checkers

playCheckers :: Game -> IO()
playCheckers game = do
    if not (checkGameOver game)
       then continueGame
       else endGame where
    continueGame = undefined
    endGame = undefined