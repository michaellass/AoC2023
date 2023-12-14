import Data.List (elemIndices, transpose)
import Data.List.Split (splitOn)

process x = sum . map (length x -) . concat $ positions
  where
    fixedRocks = elemIndices '#' x
    roundRocks = map (length . filter (== 'O')) . splitOn "#" $ x
    offsets = -1 : fixedRocks
    positions = [[o + 1 .. o + n] | (o, n) <- zip offsets roundRocks]

solve x = sum . map process $ columns
  where
    columns = transpose x

main = readFile "input" >>= print . solve . lines