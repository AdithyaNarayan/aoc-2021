{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c s = h : splitOn c t
  where
    h = takeWhile (/= c) s
    t = dropWhile (== c) (dropWhile (/= c) s)

f :: [Int] -> Int
f l = minimum $ map (\x -> sum (map (\y -> abs (x - y)) l)) [0 .. maximum l]

main :: IO ()
main = do
  file <- getContents
  let numbers = map read (splitOn ',' (head $ lines file)) :: [Int]
  print (f numbers)
