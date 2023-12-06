import Data.List.Split (splitOn)

possibleDistances time = [(x, (time - x) * x) | x <- buttonPresses]
  where
    buttonPresses = [0 .. time]

winning toBeat times = [x | (x, y) <- times, y > toBeat]

solve x = length . winning distance $ possibleDistances time
  where
    time = read . filter (/= ' ') $ head x !! 1
    distance = read . filter (/= ' ') $ x !! 1 !! 1

main = readFile "input" >>= print . solve . map (splitOn ":") . lines