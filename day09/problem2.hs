import Data.Char (ord)
import Data.List (nub, sort, transpose)

windowThree :: [Int] -> [[Int]]
windowThree [] = []
windowThree x = filter (\x -> length x == 3) $ take 3 x : windowThree (tail x)

greaterThanPrevAndNext :: [Int] -> [Bool]
greaterThanPrevAndNext x =
  map (\x -> head (tail x) < head x && head (tail x) < last x) $ windowThree (10 : x ++ [10])

heatMap :: [[Int]] -> [[Bool]]
heatMap x = map (map (uncurry (&&))) $ zipWith zip hm hmTrans
  where
    hm = map greaterThanPrevAndNext x
    hmTrans = transpose (map greaterThanPrevAndNext (transpose x))

indices :: [[a]] -> [[(Int, Int)]]
indices x = [[(i, j) | j <- [0 .. length (head x) - 1]] | i <- [0 .. length x - 1]]

getLowPoints :: [[Int]] -> [(Int, Int)]
getLowPoints graph = concatMap (map fst . filter snd) $ zipWith zip (indices graph) (heatMap graph)

get :: Int -> Int -> [[Int]] -> Int
get x y l
  | isWithinBounds x y l = l !! x !! y
  | otherwise = 10

isWithinBounds :: Int -> Int -> [[Int]] -> Bool
isWithinBounds x y l = x >= 0 && x < length l && y >= 0 && y < length (head l)

getNeighboursMoreThanSelf :: Int -> Int -> [[Int]] -> [(Int, Int)]
getNeighboursMoreThanSelf x y graph = filter (\(x1, y1) -> isWithinBounds x1 y1 graph && get x y graph < get x1 y1 graph && get x1 y1 graph /= 9) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]

dfs :: Int -> Int -> [[Int]] -> [(Int, Int)] -> [(Int, Int)]
dfs x y graph visited = if null neighbours then nub $ (x, y) : visited else nub $ (x, y) : concatMap (\(x, y) -> dfs x y graph visited) neighbours
  where
    neighbours = getNeighboursMoreThanSelf x y graph

f :: [[Int]] -> Int
f graph = product $ take 3 $ reverse $ sort $ map (\(x, y) -> length $ dfs x y graph []) (getLowPoints graph)

main :: IO ()
main = do
  file <- getContents
  let graph = map (map (abs . (-) 48 . ord)) (lines file)
  print (f graph)
