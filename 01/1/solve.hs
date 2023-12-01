import Data.Char (isDigit)

firstNumber (x : xs)
  | isDigit x = x
  | otherwise = firstNumber xs

lastNumber = firstNumber . reverse

processLine s = read [firstNumber s, lastNumber s]

main = do
  contents <- readFile "input"
  print . sum . map processLine . lines $ contents