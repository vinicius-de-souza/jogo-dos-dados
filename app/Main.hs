module Main where

import Functions (createDice, getDifficulty, getNumDice, playGame, easyComputerTurn, hardComputerTurn)

main :: IO ()
main = do
    putStrLn "\n****************************"
    putStrLn "Bem-vindo ao jogo dos dados!"
    putStrLn "****************************\n"
    difficulty <- getDifficulty
    numDice <- getNumDice
    dice <- createDice numDice
    let computerTurn = if difficulty == 1 then easyComputerTurn else hardComputerTurn
    playGame dice (difficulty == 1) computerTurn
