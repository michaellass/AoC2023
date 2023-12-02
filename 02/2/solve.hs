import Data.List.Split

parsePair [n, c]
  | c == "red" = (read n, 0, 0)
  | c == "green" = (0, read n, 0)
  | c == "blue" = (0, 0, read n)

addUp (a, b, c) (d, e, f) = (a + d, b + e, c + f)

maxOf (a, b, c) (d, e, f) = (max a d, max b e, max c f)

minimumForGame = foldl1 maxOf . map (foldl1 addUp . map (parsePair . words) . splitOn ",") . splitOn ";"

processTuple (a, b, c) = a * b * c

main = do
  contents <- readFile "input"
  print . sum . map (processTuple . minimumForGame . (!! 1) . splitOn ":") . lines $ contents