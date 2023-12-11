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

doubleEmpty lineIn
  | all (== '.') lineIn = [lineIn, lineIn]
  | otherwise = [lineIn]

distance (c1, c2) = abs (row c1 - row c2) + abs (col c1 - col c2)

solve field = sum . map distance $ pairs
  where
    expandedField = concatMap doubleEmpty . transpose . concatMap doubleEmpty . transpose $ field
    augementedField = augmentCoordinates expandedField
    galaxies = filter ((== '#') . val) . concat $ augementedField
    pairs = [(g1, g2) | g1 <- galaxies, g2 <- galaxies, g1 < g2]

main = readFile "input" >>= print . solve . lines