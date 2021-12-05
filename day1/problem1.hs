import Debug.Trace (trace)

zipWithNext :: [Int] -> [[Int]] -> [[Int]]
zipWithNext [] x = x
zipWithNext [x] acc = acc
zipWithNext (x : xs) acc = zipWithNext xs ((x : take 1 xs) : acc)

f :: [Int] -> Int
f x = sum (map (\x -> fromEnum (head x < last x)) (reverse (zipWithNext x [])))

main :: IO ()
main = do
  userInput <- getContents
  let numbers = map read (lines userInput) :: [Int]
  trace (show (f numbers)) $ return ()
