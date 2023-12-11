import Data.List (transpose)
import Data.List.Split (splitOn)

data Coord = Coord {row :: Int, col :: Int, val :: Char} deriving (Eq, Ord, Show)

getAt field row col
  | row < 0 || col < 0 = Coord {row = row, col = col, val = '.'}
  | row >= length field = Coord {row = row, col = col, val = '.'}
  | col >= length (head field) = Coord {row = row, col = col, val = '.'}
  | otherwise = (field !! row) !! col

createCoord row = map (\(col, val) -> Coord {row = row, col = col, val = val})

augmentCoordinates = zipWith createCoord [0 ..] . map (zip [0 ..])

distance rowValues colValues (c1, c2) = verticalDistance + horizontalDistance
  where
    verticalDistance
      | row c1 <= row c2 = sum . take (row c2 - row c1) . drop (row c1 + 1) $ rowValues
      | row c1 > row c2 = sum . take (row c1 - row c2) . drop (row c2 + 1) $ rowValues
    horizontalDistance
      | col c1 <= col c2 = sum . take (col c2 - col c1) . drop (col c1 + 1) $ colValues
      | col c1 > col c2 = sum . take (col c1 - col c2) . drop (col c2 + 1) $ colValues

solve field = sum $ map (distance rowValues colValues) pairs
  where
    rowValues = [if empty then 1000000 else 1 | empty <- map (all (== '.')) field]
    colValues = [if empty then 1000000 else 1 | empty <- map (all (== '.')) $ transpose field]
    augementedField = augmentCoordinates field
    galaxies = filter ((== '#') . val) . concat $ augementedField
    pairs = [(g1, g2) | g1 <- galaxies, g2 <- galaxies, g1 < g2]

main = readFile "input" >>= print . solve . lines