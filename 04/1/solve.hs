import Data.List.Split (splitOn)

processGame input = if matches == 0 then 0 else 2 ^ (matches - 1)
  where
    matches = length $ filter (`elem` winning) mine
    winning = words $ head input
    mine = words $ input !! 1

solve = sum . map (processGame . splitOn "|") . concatMap (tail . splitOn ":")

main = readFile "input" >>= print . solve . lines