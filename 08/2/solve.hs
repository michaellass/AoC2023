import Data.Char (isAlphaNum)
import Data.List.Split (splitOn)

-- Brute forcing as follows certainly does not work:

-- makeStep nodes current taken next counter
--   | all (\x -> last x == 'Z') current = counter
--   | otherwise = makeStep nodes nextNodes nextTaken nextNext (counter+1)
--   where
--     options = filter ((`elem` current) . head) nodes
--     left = map (filter isAlphaNum . (!! 2)) options
--     right = map (filter isAlphaNum . (!! 3)) options
--     direction = head next
--     nextNodes
--       | direction == 'L' = left
--       | direction == 'R' = right
--     nextNext
--       | length next == 1 = taken ++ next
--       | otherwise = tail next
--     nextTaken
--       | length next == 1 = []
--       | otherwise = taken ++ [direction]

makeStep nodes current taken next
  | last current == 'Z' = 0
  | otherwise = 1 + makeStep nodes nextNode nextTaken nextNext
  where
    options = concat $ filter ((== current) . head) nodes
    left = filter isAlphaNum $ options !! 2
    right = filter isAlphaNum $ options !! 3
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

-- ... instead, we need to assume things not explicitly stated in the task:
-- 1. Each path leads to one and only one end node. There is no overlap of paths.
-- 2. After an end node (..Z), we will always visit the start node (..A) next again. I.e.,
--    we move in circles right from the beginning and start right behind our sought-for end node.
-- 3. The circle perfectly aligns with the sequence of directions, i.e., the path does not change
--    after finishing the circle once.
-- This allows us to simply determine the path lengths for each start and end node combination and
-- compute the LCM, giving us the result.

solve x = foldl1 lcm $ [makeStep nodes startingNode [] directions | startingNode <- startingNodes]
  where
    directions = concat $ head x
    nodes = filter (/= [""]) $ tail x
    startingNodes = filter (\x -> last x == 'A') $ map head nodes

main = readFile "input" >>= print . solve . map (splitOn " ") . lines