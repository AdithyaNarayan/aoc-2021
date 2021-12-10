splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c s = h : splitOn c t
  where
    h = takeWhile (/= c) s
    t = dropWhile (== c) (dropWhile (/= c) s)

toTuple :: [a] -> (a, a)
toTuple x = (head x, last x)

f :: [([String], [String])] -> Int
f = sum . map (length . filter (\x -> x == 2 || x == 3 || x == 4 || x == 7) . map length . snd)

main :: IO ()
main = do
  file <- getContents
  let arr = map (toTuple . map words . splitOn '|') (lines file)
  print (f arr)
