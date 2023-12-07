import Data.Char (digitToInt)
import Data.List (elemIndex, group, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Ord, Eq, Show)

cardValue 'T' = 10
cardValue 'J' = 11
cardValue 'Q' = 12
cardValue 'K' = 13
cardValue 'A' = 14
cardValue x = digitToInt x

cmpHands x y
  | categorizeHand x < categorizeHand y = LT
  | categorizeHand x == categorizeHand y = compare (map cardValue x) (map cardValue y)
  | otherwise = GT

categorizeHand hand
  | 5 `elem` sameOfAKind = FiveOfAKind
  | 4 `elem` sameOfAKind = FourOfAKind
  | 3 `elem` sameOfAKind && 2 `elem` sameOfAKind = FullHouse
  | 3 `elem` sameOfAKind = ThreeOfAKind
  | length (filter (== 2) sameOfAKind) == 2 = TwoPair
  | 2 `elem` sameOfAKind = OnePair
  | otherwise = HighCard
  where
    sameOfAKind = map length . group . sort $ hand

solve x = sum $ map (\[card, bid] -> read bid * (fromJust (elemIndex card allCards) + 1)) x
  where
    allCards = sortBy cmpHands $ map head x

main = readFile "input" >>= print . solve . map (splitOn " ") . lines