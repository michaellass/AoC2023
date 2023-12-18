import Data.List.Split (splitOneOf)

goTowards 'R' (r, c, _) = (r, c + 1, 'R')
goTowards 'L' (r, c, _) = (r, c - 1, 'L')
goTowards 'U' (r, c, _) = (r - 1, c, 'U')
goTowards 'D' (r, c, _) = (r + 1, c, 'D')

opposite 'U' = 'D'
opposite 'D' = 'U'

fst3 (x, _, _) = x

snd3 (_, x, _) = x

thrd3 (_, _, x) = x

takesteps coords [] = coords
takesteps coords ((direction, steps) : remaining) = takesteps updatedCoords remaining
  where
    currentPos = head coords
    updatedCurrentPos =
      if direction `elem` "UD"
        then (fst3 currentPos, snd3 currentPos, direction)
        else currentPos
    thisEdge = take (steps + 1) $ iterate (goTowards direction) updatedCurrentPos
    updatedCoords = reverse thisEdge ++ tail coords

calcRow border row maxCol currentCol currentCount counting waitingFor
  | currentCol > maxCol = currentCount
  | (row, currentCol, waitingFor) `elem` border =
    calcRow border row maxCol (currentCol + 1) (currentCount + 1) (not counting) (opposite waitingFor)
  | any (\x -> fst3 x == row && snd3 x == currentCol) border =
    calcRow border row maxCol (currentCol + 1) (currentCount + 1) counting waitingFor
  | counting =
    calcRow border row maxCol (currentCol + 1) (currentCount + 1) counting waitingFor
  | otherwise =
    calcRow border row maxCol (currentCol + 1) currentCount counting waitingFor

calcRowWrapper border maxCol minCol row = calcRow relevantBorder row maxCol minCol 0 False firstBorderDir
  where
    relevantBorder = filter ((== row) . fst3) border
    firstBorder = minimum relevantBorder
    firstBorderDir = thrd3 firstBorder

calcArea border = sum $ map (calcRowWrapper border maxCol minCol) [minRow .. maxRow]
  where
    minRow = minimum $ map fst3 border
    maxRow = maximum $ map fst3 border
    minCol = minimum $ map snd3 border
    maxCol = maximum $ map snd3 border

visualize border = [[let elems = filter (\x -> snd3 x == c && fst3 x == r) border in if null elems then '.' else thrd3 (head elems) | c <- [minCol .. maxCol]] | r <- [minRow .. maxRow]]
  where
    minRow = minimum $ map fst3 border
    maxRow = maximum $ map fst3 border
    minCol = minimum $ map snd3 border
    maxCol = maximum $ map snd3 border

solve x = calcArea border
  where
    dataIn = map (filter (/= "") . splitOneOf " (#)") $ lines x
    preprocessed = [(head $ head x, read $ x !! 1) | x <- dataIn]
    border = tail . reverse $ takesteps [(0, 0, 'S')] preprocessed

main = readFile "input" >>= print . solve
