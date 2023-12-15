import Basement.IntegralConv (charToInt)
import Data.List.Split (splitOn)

hash = foldl (\current x -> ((current + charToInt x) * 17) `rem` 256) 0

solve = sum . map hash . splitOn "," . filter (/= '\n')

main = readFile "input" >>= print . solve