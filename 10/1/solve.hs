import Data.List.Split (splitOn)

data Coord = Coord {row :: Int, col :: Int, val :: Char} deriving (Eq, Show)

data Direction = North | East | South | West deriving (Eq, Show)

opposite North = South
opposite South = North
opposite East = West
opposite West = East

getAt field row col
  | row < 0 || col < 0 = Coord {row = row, col = col, val = '.'}
  | row >= length field = Coord {row = row, col = col, val = '.'}
  | col >= length (head field) = Coord {row = row, col = col, val = '.'}
  | otherwise = (field !! row) !! col

getNeighbor field c North = getAt field (row c - 1) (col c)
getNeighbor field c East = getAt field (row c) (col c + 1)
getNeighbor field c South = getAt field (row c + 1) (col c)
getNeighbor field c West = getAt field (row c) (col c - 1)

connectedTo '|' = [North, South]
connectedTo '-' = [East, West]
connectedTo 'L' = [North, East]
connectedTo 'J' = [North, West]
connectedTo '7' = [South, West]
connectedTo 'F' = [South, East]
connectedTo 'S' = [North, South, East, West]

connectedNeighbors field c =
  [ neighbor
    | x <- connectedTo (val c),
      let neighbor = getNeighbor field c x,
      opposite x `elem` connectedTo (val neighbor)
  ]

createCoord row = map (\(col, val) -> Coord {row = row, col = col, val = val})

augmentCoordinates = zipWith createCoord [0 ..] . map (zip [0 ..])

buildPath field steps current
  | val next == 'S' = nextSteps
  | otherwise = buildPath field nextSteps next
  where
    connections = connectedNeighbors field current
    options
      | null steps = filter (\c -> val c /= 'S') connections
      | otherwise = filter (/= last steps) connections
    next = head options
    nextSteps = steps ++ [current]

solve field = length (buildPath field [] start) `div` 2
  where
    start = head . filter (\c -> val c == 'S') . concat $ field

main = readFile "input" >>= print . solve . augmentCoordinates . lines