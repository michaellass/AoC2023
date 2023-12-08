import Data.Char (isUpper)
import Data.List.Split (splitOn)

makeStep nodes current taken next
  | current == "ZZZ" = 0
  | otherwise = 1 + makeStep nodes nextNode nextTaken nextNext
  where
    options = concat $ filter ((== current) . head) nodes
    left = filter isUpper $ options !! 2
    right = filter isUpper $ options !! 3
    direction = head next
    nextNode
      | direction == 'L' = left
      | direction == 'R' = right
    nextNext
      | length next == 1 = taken ++ next
      | otherwise = tail next
    nextTaken
      | length next == 1 = []
      | otherwise = taken ++ [direction]

solve x = makeStep nodes "AAA" [] directions
  where
    directions = concat $ head x
    nodes = filter (/= [""]) $ tail x

main = readFile "input" >>= print . solve . map (splitOn " ") . lines