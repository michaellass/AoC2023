import Data.List.Split (splitOn)

matches card = length $ filter (`elem` winning) mine
  where
    winning = words $ head content
    mine = words $ content !! 1
    content = splitOn "|" . snd $ card

process cards counts card
  | n < (length cards - 1) = process cards countsNext (cards !! n)
  | otherwise = countsNext
  where
    n = fst card
    thisCardsCounts = counts !! (n - 1)
    thisCardsMatches = matches card
    fixed = take n counts
    modified = map (+ thisCardsCounts) $ take thisCardsMatches $ drop n counts
    remainder = drop (n + thisCardsMatches) counts
    countsNext = fixed ++ modified ++ remainder

solve input = sum . process cards initial $ head cards
  where
    cards = zip [1 ..] . concatMap (tail . splitOn ":") $ input
    initial = [1 | x <- cards]

main = readFile "input" >>= print . solve . lines