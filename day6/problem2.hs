{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c s = h : splitOn c t
  where
    h = takeWhile (/= c) s
    t = dropWhile (== c) (dropWhile (/= c) s)

count :: Eq a => a -> [a] -> Int
count e x = length $ filter (== e) x

g :: Int -> [Int] -> [Int] -> Int
g 0 babies matured = sum (babies ++ matured)
g i (b : babies) (m : matured) = g (i - 1) (babies ++ [m]) (matured ++ [m + b])

f :: [Int] -> Int
f ages = g 256 [0, 0] $ map (`count` ages) [0 .. 6]

main :: IO ()
main = do
  file <- getContents
  let ages = map read (splitOn ',' (head $ lines file)) :: [Int]
  print (f ages)
