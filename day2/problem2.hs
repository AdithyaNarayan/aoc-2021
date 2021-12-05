{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

g :: [(String, Int)] -> Int -> Int -> Int -> Int
g [] h d _ = h * d
g ((s, n) : xs) h d a
  | s == "forward" = g xs (h + n) (d + a * n) a
  | s == "down" = g xs h d (a + n)
  | s == "up" = g xs h d (a - n)

f :: [(String, Int)] -> Int
f x = g x 0 0 0

main :: IO ()
main = do
  userInput <- getContents
  let arr = map ((\x -> (head x, read (last x))) . words) (lines userInput) :: [(String, Int)]
  print (f arr)
