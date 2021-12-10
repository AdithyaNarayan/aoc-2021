{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (transpose)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c s = h : splitOn c t
  where
    h = takeWhile (/= c) s
    t = dropWhile (== c) (dropWhile (/= c) s)

parseBoards :: [String] -> [[[Int]]]
parseBoards x = map (map (map read . words)) (splitOn "" x)

getBoardsAfterRound :: Int -> [[[Int]]] -> [[[Int]]]
getBoardsAfterRound draw = map (map (map (\num -> if num == draw then -1 else num)))

checkIfBoardIsDone :: [[Int]] -> Bool
checkIfBoardIsDone board = any (\row -> sum row == (- l)) board || any (\row -> sum row == (- l)) (transpose board)
  where
    l = length board

computeScore :: [[Int]] -> Int -> Int
computeScore board x = sum (map (sum . filter (/= -1)) board) * x

f :: [[[Int]]] -> [Int] -> Int
f [board] [x] = computeScore board x
f boards (x : xs)
  | null winningBoard = f boardsAfterRound xs
  | otherwise = f winningBoard [x]
  where
    boardsAfterRound = getBoardsAfterRound x boards
    winningBoard = filter checkIfBoardIsDone boardsAfterRound

main :: IO ()
main = do
  file <- getContents
  let input = lines file
  let draws = map read (splitOn ',' (head input)) :: [Int]
  let boards = parseBoards (tail input)
  print (f boards draws)
