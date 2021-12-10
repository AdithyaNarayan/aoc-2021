import Debug.Trace (trace)

threeWindow :: [Int] -> [[Int]]
threeWindow x =
  dropWhile (\x -> length x < 3) (f x [])
  where
    f :: [Int] -> [[Int]] -> [[Int]]
    f [] x = x
    f [x] acc = acc
    f (x : xs) acc = f xs ((x : take 2 xs) : acc)

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
  let n = map sum (threeWindow numbers)
  print (f (reverse n))
