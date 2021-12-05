{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

g :: [(String, Int)] -> Int -> Int -> Int
g [] h d = h * d
g ((s, n) : xs) h d
  | s == "forward" = g xs (h + n) d
  | s == "down" = g xs h (d + n)
  | s == "up" = g xs h (d - n)

f :: [(String, Int)] -> Int
f x = g x 0 0

main :: IO ()
main = do
  userInput <- getContents
  let arr = map ((\x -> (head x, read (last x))) . words) (lines userInput) :: [(String, Int)]
  print (f arr)
