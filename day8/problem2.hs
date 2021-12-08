import Data.Char (chr, ord)
import Data.List (intersect, sort)
import Debug.Trace (trace)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c s = h : splitOn c t
  where
    h = takeWhile (/= c) s
    t = dropWhile (== c) (dropWhile (/= c) s)

count :: Eq a => a -> [a] -> Int
count c x = length $ filter (== c) x

countSum :: Eq a => a -> [[a]] -> Int
countSum c x = sum $ map (count c) x

isCap :: Char -> Bool
isCap x = a >= 65 && a <= 90
  where
    a = ord x

listIntToInt :: [Int] -> Int
listIntToInt = foldl (\num digit -> digit + (num * 10)) 0

decodeAndSort :: ([String], [String]) -> [String]
decodeAndSort (x, y) = map (map chr . sort . map ord) $ drop 10 warped
  where
    cs = zip (map (`countSum` x) ['a' .. 'g']) ['a' .. 'g']
    -- BEF uniquely occurs a said number of times across all 10 digits
    warpedBEF =
      map
        ( map
            ( \x ->
                case fst $ cs !! (ord x - ord 'a') of
                  4 -> 'E'
                  6 -> 'B'
                  9 -> 'F'
                  _ -> x
            )
        )
        (x ++ y)
    -- the other non-F character in 1 (which is length 2) is C
    getC = head $ filter (/= 'F') $ head $ filter (\x -> length x == 2) (take 10 warpedBEF)
    -- A and C occur 8 times across all 10 numbers so the 8 which is not C must be A
    getA = snd $ head $ filter (\(x, y) -> x == 8 && y /= getC) cs
    --- warping to add A and C
    warpedABCEF = map (map (\x -> if x == getC then 'C' else if x == getA then 'A' else x)) warpedBEF
    -- D is the only letter so far that occurs in 4 that hasn't been decoded
    getD = head $ filter (not . isCap) $ head $ filter (\x -> length x == 4) (take 10 warpedABCEF)
    warpedABCDEF = map (map (\x -> if x == getD then 'D' else x)) warpedABCEF
    -- G is the only letter left, take from 8 which has all the letters
    getG = head $ filter (not . isCap) $ head $ filter (\x -> length x == 7) (take 10 warpedABCDEF)
    warped = map (map (\x -> if x == getG then 'G' else x)) warpedABCDEF

sevenStringToInt :: String -> Int
sevenStringToInt x
  | x == "ABCEFG" = 0
  | x == "CF" = 1
  | x == "ACDEG" = 2
  | x == "ACDFG" = 3
  | x == "BCDF" = 4
  | x == "ABDFG" = 5
  | x == "ABDEFG" = 6
  | x == "ACF" = 7
  | x == "ABCDEFG" = 8
  | x == "ABCDFG" = 9
  | otherwise = -1

f :: [([String], [String])] -> Int
f = sum . map ((listIntToInt . map sevenStringToInt) . decodeAndSort)

main :: IO ()
main = do
  file <- getContents
  let arr = map ((\x -> (head x, last x)) . map words . splitOn '|') (lines file)
  print (f arr)
