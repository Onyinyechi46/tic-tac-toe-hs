-- Tic Tac Toe in Haskell
module Main where

import Data.Maybe (isJust, fromMaybe)
import Data.Char (toUpper)
import Control.Monad (when)
import Control.Applicative ((<|>))


-- Players
data Player = X | O deriving (Eq, Show)

-- Board is 3x3 grid of Maybe Player
type Board = [[Maybe Player]]

-- Create empty board
emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)

-- Switch player
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

-- Show the board
showBoard :: Board -> String
showBoard b =
  unlines [unwords [cellToStr c | c <- row] | row <- b]
  where
    cellToStr Nothing  = "."
    cellToStr (Just p) = show p

-- Place a mark
makeMove :: Board -> Player -> (Int, Int) -> Maybe Board
makeMove b p (r, c)
  | r < 0 || r > 2 || c < 0 || c > 2 = Nothing
  | isJust ((b !! r) !! c)           = Nothing
  | otherwise =
      let newRow = take c (b !! r) ++ [Just p] ++ drop (c+1) (b !! r)
      in Just (take r b ++ [newRow] ++ drop (r+1) b)

-- Check winner
checkWinner :: Board -> Maybe Player
checkWinner b =
  let linesToCheck =
        rows ++ cols ++ diags
      rows  = b
      cols  = [[b !! r !! c | r <- [0..2]] | c <- [0..2]]
      diags = [[b !! i !! i | i <- [0..2]],
               [b !! i !! (2-i) | i <- [0..2]]]
      winnerLine line =
        case sequence line of
          Just [p1,p2,p3] | p1 == p2 && p2 == p3 -> Just p1
          _ -> Nothing
  in foldr (\line acc -> acc <|> winnerLine line) Nothing linesToCheck

-- Play loop
playGame :: Board -> Player -> IO ()
playGame board player = do
  putStrLn $ "\nCurrent board:\n" ++ showBoard board
  case checkWinner board of
    Just p  -> putStrLn $ "Player " ++ show p ++ " wins!"
    Nothing ->
      if all isJust (concat board)
        then putStrLn "It's a draw!"
        else do
          putStrLn $ "Player " ++ show player ++ ", enter row and column (0-2):"
          input <- getLine
          let parts = words input
          if length parts /= 2 then do
            putStrLn "Invalid input. Please enter two numbers like: 1 2"
            playGame board player
          else do
            let [r, c] = map read parts
            case makeMove board player (r, c) of
              Nothing     -> do
                putStrLn "Invalid move, try again."
                playGame board player
              Just newB -> playGame newB (nextPlayer player)

-- Entry point
main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe!"
  playGame emptyBoard X
