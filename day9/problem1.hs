import Data.Char (ord)
import Data.List (transpose)

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

f :: [[Int]] -> Int
f x = sum $ concatMap (map ((+ 1) . fst) . filter snd) (zipWith zip x (heatMap x))

main :: IO ()
main = do
  file <- getContents
  let arr = map (map (abs . (-) 48 . ord)) (lines file)
  print (f arr)
