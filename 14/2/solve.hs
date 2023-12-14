import Data.List (elemIndices, transpose)
import Data.List.Split (splitOn)

tilt x =
  [ if i `elem` positions
      then 'O'
      else
        if i `elem` fixedRocks
          then '#'
          else '.'
    | i <- [0 .. length x - 1]
  ]
  where
    fixedRocks = elemIndices '#' x
    roundRocks = map (length . filter (== 'O')) . splitOn "#" $ x
    offsets = -1 : fixedRocks
    positions = concat $ [[o + 1 .. o + n] | (o, n) <- zip offsets roundRocks]

tiltNorth = transpose . map tilt . transpose

tiltWest = map tilt

tiltSouth = transpose . map (reverse . tilt . reverse) . transpose

tiltEast = map (reverse . tilt . reverse)

performCycle = tiltEast . tiltSouth . tiltWest . tiltNorth

weightOfRow x = sum . map (length x -) . elemIndices 'O' $ x

northboundWeight = sum . map weightOfRow . transpose

billionCycles states
  | dupOf /= [] = northboundWeight $ states !! relevantIndex
  | otherwise = billionCycles (states ++ [nextX])
  where
    nextX = performCycle (last states)
    dupOf = elemIndices nextX states
    cycleLength = length states - head dupOf
    relevantIndex = head dupOf + (1000000000 - head dupOf) `rem` cycleLength

solve x = billionCycles [x]

main = readFile "input" >>= print . solve . lines