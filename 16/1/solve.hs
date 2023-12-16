import Data.Maybe (mapMaybe)

data Coord = Coord {row :: Int, col :: Int, val :: Char, ltr :: Bool, rtl :: Bool, ttb :: Bool, btt :: Bool} deriving (Eq, Ord, Show)

getAt field row col
  | row < 0 || col < 0 = Coord {row = row, col = col, val = '.', ltr = True, rtl = True, ttb = True, btt = True}
  | row >= length field = Coord {row = row, col = col, val = '.', ltr = True, rtl = True, ttb = True, btt = True}
  | col >= length (head field) = Coord {row = row, col = col, val = '.', ltr = True, rtl = True, ttb = True, btt = True}
  | otherwise = (field !! row) !! col

data Direction = LeftD | RightD | UpD | DownD deriving (Eq, Show)

out '|' DownD = [DownD]
out '|' UpD = [UpD]
out '|' LeftD = [UpD, DownD]
out '|' RightD = [UpD, DownD]
out '-' DownD = [LeftD, RightD]
out '-' UpD = [LeftD, RightD]
out '-' LeftD = [LeftD]
out '-' RightD = [RightD]
out '/' UpD = [RightD]
out '/' DownD = [LeftD]
out '/' LeftD = [DownD]
out '/' RightD = [UpD]
out '\\' UpD = [LeftD]
out '\\' DownD = [RightD]
out '\\' LeftD = [UpD]
out '\\' RightD = [DownD]
out '.' inD = [inD]

updateCoordInField
  f
  Coord {row = r, col = c, val = v, ltr = ltr, rtl = rtl, ttb = ttb, btt = btt} =
    take r f
      ++ [ take c effectedRow
             ++ [ Coord {row = r, col = c, val = v, ltr = ltr, rtl = rtl, ttb = ttb, btt = btt}
                ]
             ++ drop (c + 1) effectedRow
         ]
      ++ drop (r + 1) f
    where
      effectedRow = f !! r

fieldToStrings = map (map (\c -> if ltr c || rtl c || ttb c || btt c then '#' else '.'))

updateCoord c UpD = Coord {row = row c, col = col c, val = val c, ltr = ltr c, rtl = rtl c, ttb = ttb c, btt = True}
updateCoord c DownD = Coord {row = row c, col = col c, val = val c, ltr = ltr c, rtl = rtl c, ttb = True, btt = btt c}
updateCoord c LeftD = Coord {row = row c, col = col c, val = val c, ltr = ltr c, rtl = True, ttb = ttb c, btt = btt c}
updateCoord c RightD = Coord {row = row c, col = col c, val = val c, ltr = True, rtl = rtl c, ttb = ttb c, btt = btt c}

updatedNext f c dir = if next /= updatedNext then Just (updatedNext, dir) else Nothing
  where
    next = case dir of
      UpD -> getAt f (row c - 1) (col c)
      DownD -> getAt f (row c + 1) (col c)
      LeftD -> getAt f (row c) (col c - 1)
      RightD -> getAt f (row c) (col c + 1)
    updatedNext = updateCoord next dir

step f (c, d) = mapMaybe (updatedNext f c) $ out (val c) d

process field [] = field
process field toEvaluate
  | null nextDirs = updatedNext
  | otherwise = process updatedNext nextDirs
  where
    nextDirs = concatMap (step field) toEvaluate
    updatedNext = foldl updateCoordInField field $ map fst nextDirs

createCoord row = map (\(col, val) -> Coord {row = row, col = col, val = val, ltr = False, rtl = False, ttb = False, btt = False})

augmentCoordinates = zipWith createCoord [0 ..] . map (zip [0 ..])

markVisited f (c, d) = updateCoordInField f $ updateCoord c d

solve x = length . filter (== '#') . concat . fieldToStrings $ process initialField [initialDir]
  where
    field = augmentCoordinates $ lines x
    initialDir = (head . head $ field, RightD)
    initialField = markVisited field initialDir

main = readFile "input" >>= print . solve