import Data.List (nub, transpose)
import Debug.Trace (trace)
import Distribution.Simple.Utils (xargs)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c s = h : splitOn c t
  where
    h = takeWhile (/= c) s
    t = dropWhile (== c) (dropWhile (/= c) s)

toInt :: String -> Int
toInt = read

toTuplePair :: [a] -> (a, a)
toTuplePair x = (head x, last x)

pointFromString :: String -> (Int, Int)
pointFromString x = toTuplePair $ map toInt (splitOn ',' x)

parseInput :: [String] -> [((Int, Int), (Int, Int))]
parseInput = map (toTuplePair . map (pointFromString . filter (/= '>') . filter (/= ' ')) . splitOn '-')

genList :: Int -> Int -> [Int]
genList x y
  | x > y = [y .. x]
  | otherwise = [x .. y]

lineFromTwoPoints :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
lineFromTwoPoints ((x1, y1), (x2, y2))
  | x1 == x2 = zip (replicate (abs (y2 - y1) + 1) x1) (genList y1 y2)
  | y1 == y2 = zip (genList x1 x2) (replicate (abs (x2 - x1) + 1) y1)
  | abs (x2 - x1) == abs (y2 - y1) =
    if (x2 - x1) `div` (y2 - y1) < 0
      then zip (genList x1 x2) (reverse (genList y1 y2))
      else zip (genList x1 x2) (genList y1 y2)
  | otherwise = []

create2D :: Int -> [[Int]]
create2D x = replicate x (replicate x 0)

replaceInList :: Int -> a -> [a] -> [a]
replaceInList _ _ [] = error "IndexOutOfBounds"
replaceInList 0 e (_ : xs) = e : xs
replaceInList i e (x : xs) = x : replaceInList (i - 1) e xs

replaceIn2DList :: Int -> Int -> a -> [[a]] -> [[a]]
replaceIn2DList _ _ _ [] = error "IndexOutOfBounds"
replaceIn2DList 0 j e (x : xs) = replaceInList j e x : xs
replaceIn2DList i j e (x : xs) = x : replaceIn2DList (i - 1) j e xs

setPointInGraph :: [[Int]] -> (Int, Int) -> [[Int]]
setPointInGraph g (x, y) = replaceIn2DList x y (curr + 1) g
  where
    curr = g !! x !! y

f :: [((Int, Int), (Int, Int))] -> Int
f l = length $ filter (> 1) $ concat $ foldl setPointInGraph (create2D (m + 1)) points
  where
    points = concatMap lineFromTwoPoints l
    m = foldl (\m ((x1, y1), (x2, y2)) -> maximum [m, x1, y1, x2, y2]) 0 l

main :: IO ()
main = do
  file <- getContents
  let input = lines file
  let points = parseInput input
  print (f points)
