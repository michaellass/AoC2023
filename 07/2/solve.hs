import Data.Char (digitToInt)
import Data.List (elemIndex, group, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Ord, Eq, Show)

cardValue 'T' = 10
cardValue 'J' = 1
cardValue 'Q' = 12
cardValue 'K' = 13
cardValue 'A' = 14
cardValue x = digitToInt x

cmpHands x y
  | categorizeHand x < categorizeHand y = LT
  | categorizeHand x == categorizeHand y = compare (map cardValue x) (map cardValue y)
  | otherwise = GT

categorizeHand hand
  | 5 `elem` sameOfAKindWithJokers = FiveOfAKind
  | 4 `elem` sameOfAKindWithJokers = FourOfAKind
  | 3 `elem` sameOfAKindWithJokers && 2 `elem` sameOfAKindWithJokers = FullHouse
  | 3 `elem` sameOfAKindWithJokers = ThreeOfAKind
  | length (filter (== 2) sameOfAKindWithJokers) == 2 = TwoPair
  | 2 `elem` sameOfAKindWithJokers = OnePair
  | otherwise = HighCard
  where
    jokers = length . filter (== 'J') $ hand
    sameOfAKind =
      if hand == "JJJJJ"
        then [5]
        else map length . group . sort . filter (/= 'J') $ hand
    largestGroup = fromJust $ elemIndex (maximum sameOfAKind) sameOfAKind
    sameOfAKindWithJokers =
      if hand == "JJJJJ"
        then [5]
        else take largestGroup sameOfAKind ++ [(sameOfAKind !! largestGroup) + jokers] ++ drop (largestGroup + 1) sameOfAKind

solve x = sum $ map (\[card, bid] -> read bid * (fromJust (elemIndex card allCards) + 1)) x
  where
    allCards = sortBy cmpHands $ map head x

main = readFile "input" >>= print . solve . map (splitOn " ") . lines