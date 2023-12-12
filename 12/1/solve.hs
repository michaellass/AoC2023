import Data.List (elemIndex, group)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

splitLine l = (head parts, map read . splitOn "," $ parts !! 1)
  where
    parts = splitOn " " l

evaluate = map length . filter (all (== '#')) . group

allPossible x
  | '?' `elem` x = [start ++ middle : remainder | middle <- ".#", remainder <- remainders]
  | otherwise = [x]
  where
    idx = fromJust $ elemIndex '?' x
    start = take idx x
    remainders = allPossible $ drop (idx + 1) x

solve = sum . map ((\l -> length . filter (== snd l) . map evaluate . allPossible . fst $ l) . splitLine)

main = readFile "input" >>= print . solve . lines