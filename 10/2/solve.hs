import Data.List.Split (splitOn)
import qualified Data.Set as Set

data Coord = Coord {row :: Int, col :: Int, val :: Char} deriving (Eq, Ord, Show)

data Direction = North | East | South | West | NorthEast | SouthEast | SouthWest | NorthWest deriving (Eq, Ord, Show)

opposite North = South
opposite South = North
opposite East = West
opposite West = East

coordDiffToDir (-1) 0 = North
coordDiffToDir 0 (-1) = West
coordDiffToDir 0 1 = East
coordDiffToDir 1 0 = South

getAt field row col
  | row < 0 || col < 0 = Coord {row = row, col = col, val = '.'}
  | row >= length field = Coord {row = row, col = col, val = '.'}
  | col >= length (head field) = Coord {row = row, col = col, val = '.'}
  | otherwise = (field !! row) !! col

validCoord field coord = (row coord >= 0) && (col coord >= 0) && (row coord < length field) && (col coord < length (head field))

getNeighbor field c North = getAt field (row c - 1) (col c)
getNeighbor field c East = getAt field (row c) (col c + 1)
getNeighbor field c South = getAt field (row c + 1) (col c)
getNeighbor field c West = getAt field (row c) (col c - 1)
getNeighbor field c NorthEast = getAt field (row c - 1) (col c + 1)
getNeighbor field c SouthEast = getAt field (row c + 1) (col c + 1)
getNeighbor field c SouthWest = getAt field (row c + 1) (col c - 1)
getNeighbor field c NorthWest = getAt field (row c - 1) (col c - 1)

connectedTo '|' = [North, South]
connectedTo '-' = [East, West]
connectedTo 'L' = [North, East]
connectedTo 'J' = [North, West]
connectedTo '7' = [South, West]
connectedTo 'F' = [South, East]
connectedTo 'S' = [North, South, East, West]

-- left-hand side and right-hand side when moving through a pipe
-- passed direction is moving direction when entering the pipe
adjacentTo '|' North = ([West], [East])
adjacentTo '|' South = ([East], [West])
adjacentTo '-' East = ([North], [South])
adjacentTo '-' West = ([South], [North])
adjacentTo 'L' South = ([], [West, SouthWest, South])
adjacentTo 'L' West = ([West, SouthWest, South], [])
adjacentTo 'J' East = ([], [South, SouthEast, East])
adjacentTo 'J' South = ([South, SouthEast, East], [])
adjacentTo '7' North = ([], [East, NorthEast, North])
adjacentTo '7' East = ([North, NorthEast, East], [])
adjacentTo 'F' North = ([West, NorthWest, North], [])
adjacentTo 'F' West = ([], [West, NorthWest, North])

connectedNeighbors field c =
  [ (neighbor, x)
    | x <- connectedTo (val c),
      let neighbor = getNeighbor field c x,
      opposite x `elem` connectedTo (val neighbor)
  ]

createCoord row = map (\(col, val) -> Coord {row = row, col = col, val = val})

augmentCoordinates = zipWith createCoord [0 ..] . map (zip [0 ..])

buildPath field steps leftHandSide rightHandSide current
  | val (fst next) == 'S' = (nextSteps, leftHandSide, rightHandSide)
  | otherwise = buildPath field nextSteps nextLeftHandSide nextRightHandSide (fst next)
  where
    connections = connectedNeighbors field current
    options
      | null steps = filter (\c -> val (fst c) /= 'S') connections
      | otherwise = filter (\c -> fst c /= last steps) connections
    next = head options
    nextSteps = steps ++ [current]
    nextLeftHandSide =
      Set.union leftHandSide
        . Set.fromList
        . filter (validCoord field)
        . map (getNeighbor field (fst next))
        . fst
        $ adjacentTo (val (fst next)) (snd next)
    nextRightHandSide =
      Set.union rightHandSide
        . Set.fromList
        . filter (validCoord field)
        . map (getNeighbor field (fst next))
        . snd
        $ adjacentTo (val (fst next)) (snd next)

adjacentToS field s next last = (Set.fromList lhs, Set.fromList rhs)
  where
    directionNext = coordDiffToDir (row next - row s) (col next - col s)
    directionLast = coordDiffToDir (row last - row s) (col last - col s)
    enteringDirection = opposite directionLast
    kind = head [x | x <- "|-LJ7F", Set.fromList [directionLast, directionNext] == Set.fromList (connectedTo x)]
    (lhsDirs, rhsDirs) = adjacentTo kind enteringDirection
    lhs = filter (validCoord field) $ map (getNeighbor field s) lhsDirs
    rhs = filter (validCoord field) $ map (getNeighbor field s) rhsDirs

grow field pathNodes nodes
  | Set.map (validCoord field) newNodes /= Set.fromList [True] = Set.empty
  | newNodes == nodes = nodes
  | otherwise = grow field pathNodes newNodes
  where
    nodeNeighbors n = Set.fromList [getNeighbor field n dir | dir <- [North, South, West, East]] `Set.difference` pathNodes
    newNodes = foldl Set.union nodes $ Set.map nodeNeighbors nodes

solve field = sum $ map (length . grow field pathNodes) [group1, group2]
  where
    start = head . filter (\c -> val c == 'S') . concat $ field
    (path, lhs, rhs) = buildPath field [] Set.empty Set.empty start
    pathNodes = Set.fromList path
    (lhsS, rhsS) = adjacentToS field (head path) (head (tail path)) (last path)
    group1 = (lhs `Set.union` lhsS) `Set.difference` pathNodes
    group2 = (rhs `Set.union` rhsS) `Set.difference` pathNodes

main = readFile "input" >>= print . solve . augmentCoordinates . lines