import Data.Char (isDigit)
import Data.List (nub)

data Coord = Coord {row :: Int, col :: Int, val :: Char} deriving (Eq, Show)

getAt field row col
  | row < 0 || col < 0 = Coord {row = row, col = col, val = '.'}
  | row >= length field = Coord {row = row, col = col, val = '.'}
  | col >= length (head field) = Coord {row = row, col = col, val = '.'}
  | otherwise = (field !! row) !! col

createCoord row = map (\(col, val) -> Coord {row = row, col = col, val = val})

adjacent f e = [getAt f r c | r <- [row e - 1 .. row e + 1], c <- [col e - 1 .. col e + 1]]

augmentCoordinates = zipWith createCoord [0 ..] . map (zip [0 ..])

digitToNumber f e = adjacentDigitsLeft f e ++ [e] ++ adjacentDigitsRight f e

adjacentDigits f pos = filter (isDigit . val) $ adjacent f pos

adjacentNumbers f = nub . map (digitToNumber f) . adjacentDigits f

isGear f e = (val e == '*') && length (adjacentNumbers f e) == 2

adjacentDigitsRight f e
  | isDigit (val next) = next : adjacentDigitsRight f next
  | otherwise = []
  where
    next = getAt f (row e) (col e + 1)

adjacentDigitsLeft f e
  | isDigit (val prev) = adjacentDigitsLeft f prev ++ [prev]
  | otherwise = []
  where
    prev = getAt f (row e) (col e - 1)

coordsToNumber = read . map val

processGear f = map coordsToNumber . adjacentNumbers f

processField f = sum . map (product . processGear f) . filter (isGear f) . concat $ f

main = readFile "input" >>= print . processField . augmentCoordinates . lines