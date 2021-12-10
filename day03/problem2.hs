import Data.Char (chr)
import Data.List (transpose)
import Debug.Trace (trace)

binToDec :: Int -> Int
binToDec 0 = 0
binToDec 1 = 1
binToDec n = mod n 10 + 2 * binToDec (div n 10)

count :: Eq a => [a] -> a -> Int
count x e = length (filter (== e) x)

getXBitInRow :: [String] -> Int -> (Int -> Int -> Bool) -> Char
getXBitInRow x i comp = chr (fromEnum (comp (count t '0') (div (length t) 2)) + 48)
  where
    t = transpose x !! i

stat :: [String] -> Int -> (Int -> Int -> Bool) -> String
stat [x] _ _ = x
stat x i comp = stat (filter (\x -> x !! i == y) x) (i + 1) comp
  where
    y = getXBitInRow x i comp

f :: [String] -> Int
f x = binToDec (read (stat x 0 (<=))) * binToDec (read (stat x 0 (>)))

main :: IO ()
main = do
  userInput <- getContents
  let numbers = lines userInput
  print (f numbers)
