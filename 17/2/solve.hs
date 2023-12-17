{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char (digitToInt)
import Data.Maybe (fromJust, isJust, isNothing)

data Coord = Coord {row :: Int, col :: Int, val :: Int, sh :: Int, sv :: Int} deriving (Eq, Ord, Show)

data Direction = West | North | East | South deriving (Eq, Ord, Show, Enum)

turnRight South = West
turnRight x = succ x

turnLeft West = South
turnLeft x = pred x

orthogonal x = [turnLeft x, turnRight x]

opposite = turnLeft . turnLeft

getAt field row col
  | row < 0 || col < 0 || row >= length field || col >= length (head field) = Nothing
  | otherwise = Just ((field !! row) !! col)

getNeighbor f c dir = case dir of
  North -> getAt f (row c - 1) (col c)
  South -> getAt f (row c + 1) (col c)
  West -> getAt f (row c) (col c - 1)
  East -> getAt f (row c) (col c + 1)

createCoord row = map (\(col, val) -> Coord {row = row, col = col, val = val, sh = maxBound, sv = maxBound})

augmentCoordinates = zipWith createCoord [0 ..] . map (zip [0 ..])

updateCoordInField field coord =
  take r field
    ++ [take c effectedRow ++ [coord] ++ drop (c + 1) effectedRow]
    ++ drop (r + 1) field
  where
    r = row coord
    c = col coord
    effectedRow = field !! r

updateCoordSH
  Coord {row = r, col = c, val = v, sh = sh, sv = sv}
  newSH
    | sh > newSH = Just Coord {row = r, col = c, val = v, sh = newSH, sv = sv}
    | otherwise = Nothing

updateCoordSV
  Coord {row = r, col = c, val = v, sh = sh, sv = sv}
  newSV
    | sv > newSV = Just Coord {row = r, col = c, val = v, sh = sh, sv = newSV}
    | otherwise = Nothing

updateFromDirection North = updateCoordSV
updateFromDirection South = updateCoordSV
updateFromDirection West = updateCoordSH
updateFromDirection East = updateCoordSH

process :: [[Coord]] -> Int -> [(Coord, Direction, Int)] -> [[Coord]]
process f distance [] = f
process f distance (next : remaining)
  | newDistance >= limit = process f distance remaining
  | isNothing newC = process straightFromHere distance remaining
  | straightSteps < 3 = process straightFromHere distance remaining
  | otherwise = process allFromHere distance remaining
  where
    end = last (last f)
    limit = minimum [sh end, sv end]
    (c, dir, straightSteps) = next
    newDistance = distance + val c
    newC = updateFromDirection dir c newDistance
    updatedF = updateCoordInField f (fromJust newC)
    nextStraight
      | straightSteps == 9 = []
      | isNothing neighbor = []
      | otherwise = [(fromJust neighbor, dir, straightSteps + 1)]
      where
        neighbor = getNeighbor f c dir
    nextOrtho =
      -- I'm sure there is a better way to express this:
      concatMap
        ( \d ->
            let neighbor = getNeighbor f c d
             in [(fromJust neighbor, d, 0) | isJust neighbor]
        )
        $ orthogonal dir
    straightFromHere = process f newDistance nextStraight
    allFromHere = process updatedF newDistance (nextStraight ++ nextOrtho)

solve x = minimum [sh end, sv end]
  where
    field = augmentCoordinates . map (map digitToInt) $ lines x
    s1 = head (head field)
    res1 = process field (-val s1) [(s1, East, 0)]
    s2 = head (head res1)
    res2 = process res1 (-val s2) [(s2, South, 0)]
    end = last (last res2)

main = readFile "input" >>= print . solve
