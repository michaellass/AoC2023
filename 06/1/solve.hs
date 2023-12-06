import Data.List.Split (splitOn)

possibleDistances time = [(x, (time - x) * x) | x <- buttonPresses]
  where
    buttonPresses = [0 .. time]

winning toBeat times = [x | (x, y) <- times, y > toBeat]

solve x = product . map length . zipWith winning distances $ map possibleDistances times
  where
    times = map read . filter (/= "") . splitOn " " $ head x !! 1
    distances = map read . filter (/= "") . splitOn " " $ x !! 1 !! 1

main = readFile "input" >>= print . solve . map (splitOn ":") . lines