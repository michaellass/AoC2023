import Data.List.Split

possible [n, c] =
  (read n <= 12 && c == "red")
    || (read n <= 13 && c == "green")
    || (read n <= 14 && c == "blue")

validateRound = all (possible . words) . splitOn ","

validateGame = all validateRound . splitOn ";"

processGame [title, results]
  | validateGame results = read . drop 5 $ title
  | otherwise = 0

main = do
  contents <- readFile "input"
  print . sum . map (processGame . splitOn ":") . lines $ contents