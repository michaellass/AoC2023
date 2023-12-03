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

isSymbol c = c `notElem` "0123456789."

allSymbols = filter (isSymbol . val) . concat

adjacentDigits f pos = filter (isDigit . val) $ adjacent f pos

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

processField f = sum . map coordsToNumber . nub . map (digitToNumber f) . concatMap (adjacentDigits f) $ allSymbols f

main = readFile "input" >>= print . processField . augmentCoordinates . lines