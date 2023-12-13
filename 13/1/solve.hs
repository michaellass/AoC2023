import Data.List (transpose)
import Data.List.Split (splitOn)

isMirror field linesAbove = relevantField == reverse relevantField
  where
    linesBelow = length field - linesAbove
    relevantLines = min linesAbove linesBelow
    relevantField = take (2 * relevantLines) . drop (linesAbove - relevantLines) $ field

flagMirrors field = map (isMirror field) [1 .. length field - 1]

listOfMirrors = map fst . filter snd . zip [1 ..] . flagMirrors

processField f = sum (listOfMirrors $ transpose f) + 100 * sum (listOfMirrors f)

solve = sum . map processField

main = readFile "input" >>= print . solve . splitOn [""] . lines