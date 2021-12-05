import Data.List (transpose)

binToDec :: Int -> Int
binToDec 0 = 0
binToDec 1 = 1
binToDec n = mod n 10 + 2 * binToDec (div n 10)

count :: Eq a => [a] -> a -> Int
count x e = length (filter (== e) x)

concatDigitsToNum :: [Int] -> Int
concatDigitsToNum = foldl (\x y -> x * 10 + y) 0

getGamma :: [String] -> Int
getGamma x = binToDec (concatDigitsToNum (map (\x -> fromEnum (count x '0' < div (length x) 2)) (transpose x)))

getEpsilon :: [String] -> Int
getEpsilon x = binToDec (concatDigitsToNum (map (\x -> fromEnum (count x '0' > div (length x) 2)) (transpose x)))

f :: [String] -> Int
f x = getGamma x * getEpsilon x

main :: IO ()
main = do
  userInput <- getContents
  let numbers = lines userInput
  print (f numbers)
