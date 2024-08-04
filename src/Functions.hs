-- src/Functions.hs

module Functions (
    Dice(..),
    createDice,
    getDifficulty,
    getNumDice,
    playGame,
    easyComputerTurn,
    hardComputerTurn
) where

import System.Random (randomRIO)
import Data.List (delete)
import Control.Concurrent (threadDelay)
import Text.Read (readMaybe)

-- Tipo representando um dado
data Dice = Dice { face :: Int } deriving (Show, Eq)

-- Função para criar uma lista de dados
createDice :: Int -> IO [Dice]
createDice n = createUniqueDice n [1..6]

-- Função auxiliar para criar dados únicos
createUniqueDice :: Int -> [Int] -> IO [Dice]
createUniqueDice 0 _ = return []
createUniqueDice n faces = do
    index <- randomRIO (0, length faces - 1)
    let selectedFace = faces !! index
    rest <- createUniqueDice (n - 1) (delete selectedFace faces)
    return (createDiceFace selectedFace : rest)

-- Função para criar um dado com uma face específica
createDiceFace :: Int -> Dice
createDiceFace value = Dice value

-- Função para rotacionar o dado
rotateDice :: Dice -> IO Dice
rotateDice (Dice 1) = return (Dice 1)
rotateDice (Dice n) = do
    let possibleFaces = case n of
            6 -> [2, 3, 4, 5]
            5 -> [1, 3, 4]
            4 -> [1, 2]
            3 -> [1, 2]
            2 -> [1]
            _ -> []
    newFace <- randomRIO (0, length possibleFaces - 1)
    return (Dice (possibleFaces !! newFace))

-- Função para imprimir o estado atual dos dados
printDice :: [Dice] -> IO ()
printDice dice = putStrLn $ "\nEstado atual dos dados:\n" ++ unwords (map (show . face) dice)

-- Função para selecionar um dado e rotacionar ou remover
selectDice :: Int -> [Dice] -> IO [Dice]
selectDice idx dice = do
    let selectedDice = dice !! idx
    if face selectedDice == 1
        then return (delete selectedDice dice)
        else do
            newDice <- rotateDice selectedDice
            return $ take idx dice ++ [newDice] ++ drop (idx + 1) dice

-- Função para ler um inteiro
readInt :: IO (Maybe Int)
readInt = do
    input <- getLine
    return $ readMaybe input

-- Função para ler e validar user input 
getValidInput :: String -> (Int -> Bool) -> IO Int
getValidInput prompt isValid = do
    putStrLn prompt
    maybeInput <- readInt
    case maybeInput of
        Just n | isValid n -> return n
        _ -> do
            putStrLn "\nEntrada inválida. Tente novamente."
            getValidInput prompt isValid

-- Função para ler e validar a dificuldade
getDifficulty :: IO Int
getDifficulty = getValidInput "Selecione o nível de dificuldade. 1 para FÁCIL, 2 para DIFÍCIL." (\n -> n == 1 || n == 2)

-- Função para ler e validar o número de dados
getNumDice :: IO Int
getNumDice = getValidInput "\nCom quantos dados você quer jogar? (mínimo 2, máximo 6):" (\n -> n >= 2 && n <= 6)

-- Função para jogada do player
userTurn :: [Dice] -> IO [Dice]
userTurn dice = do
    putStrLn "\nSelecione um dado (índice partindo de 0):"
    maybeIdx <- readInt
    case maybeIdx of
        Nothing -> do
            putStrLn "\nEntrada inválida. Tente novamente."
            userTurn dice
        Just idx -> if idx < 0 || idx >= length dice
            then do
                putStrLn "\nÍndice fora do intervalo. Tente novamente."
                userTurn dice
            else selectDice idx dice

-- Função principal do jogo
playGame :: [Dice] -> Bool -> ([Dice] -> IO [Dice]) -> IO ()
playGame dice userTurnFlag computerTurn = do
    if null dice
    then putStrLn $ if userTurnFlag then "\nO computador venceu!" else "\nVocê venceu!"
    else do
        printDice dice
        newDice <- if userTurnFlag
                   then userTurn dice
                   else computerTurn dice
        playGame newDice (not userTurnFlag) computerTurn

-- Função de nível fácil
easyComputerTurn :: [Dice] -> IO [Dice]
easyComputerTurn dice = do
    putStrLn "\nComputador jogando..."
    threadDelay (3 * 1000000)
    idx <- randomRIO (0, length dice - 1)
    selectDice idx dice

-- TODO: Implementar a função de nível difícil
hardComputerTurn :: [Dice] -> IO [Dice]
hardComputerTurn = easyComputerTurn
