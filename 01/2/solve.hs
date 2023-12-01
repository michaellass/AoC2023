import Data.Char (isDigit)
import Data.List (isPrefixOf)

replaceNumbers s
  | null s = s
  | "one" `isPrefixOf` s = '1' : replaceNumbers (tail s)
  | "two" `isPrefixOf` s = '2' : replaceNumbers (tail s)
  | "three" `isPrefixOf` s = '3' : replaceNumbers (tail s)
  | "four" `isPrefixOf` s = '4' : replaceNumbers (tail s)
  | "five" `isPrefixOf` s = '5' : replaceNumbers (tail s)
  | "six" `isPrefixOf` s = '6' : replaceNumbers (tail s)
  | "seven" `isPrefixOf` s = '7' : replaceNumbers (tail s)
  | "eight" `isPrefixOf` s = '8' : replaceNumbers (tail s)
  | "nine" `isPrefixOf` s = '9' : replaceNumbers (tail s)
  | otherwise = head s : replaceNumbers (tail s)

firstNumber (x : xs)
  | isDigit x = x
  | otherwise = firstNumber xs

lastNumber = firstNumber . reverse

processLine s = read [firstNumber s, lastNumber s]

main = do
  contents <- readFile "input"
  print . sum . map (processLine . replaceNumbers) . lines $ contents