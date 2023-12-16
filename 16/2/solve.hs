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

combineCoords
  Coord {row = r1, col = c1, val = v1, ltr = ltr1, rtl = rtl1, ttb = ttb1, btt = btt1}
  Coord {row = r2, col = c2, val = v2, ltr = ltr2, rtl = rtl2, ttb = ttb2, btt = btt2} =
    Coord
      { row = r1,
        col = c1,
        val = v1,
        ltr = ltr1 || ltr2,
        rtl = rtl1 || rtl2,
        ttb = ttb1 || ttb2,
        btt = btt1 || btt2
      }

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

updateCoordsInField = foldl updateCoordInField

fieldToStrings = map (map (\c -> if ltr c || rtl c || ttb c || btt c then '#' else '.'))

updatedNext f c UpD = if next /= updatedNext then Just (updatedNext, UpD) else Nothing
  where
    next = getAt f (row c - 1) (col c)
    updatedNext = Coord {row = row next, col = col next, val = val next, ltr = ltr next, rtl = rtl next, ttb = ttb next, btt = True}
updatedNext f c DownD = if next /= updatedNext then Just (updatedNext, DownD) else Nothing
  where
    next = getAt f (row c + 1) (col c)
    updatedNext = Coord {row = row next, col = col next, val = val next, ltr = ltr next, rtl = rtl next, ttb = True, btt = btt next}
updatedNext f c LeftD = if next /= updatedNext then Just (updatedNext, LeftD) else Nothing
  where
    next = getAt f (row c) (col c - 1)
    updatedNext = Coord {row = row next, col = col next, val = val next, ltr = ltr next, rtl = True, ttb = ttb next, btt = btt next}
updatedNext f c RightD = if next /= updatedNext then Just (updatedNext, RightD) else Nothing
  where
    next = getAt f (row c) (col c + 1)
    updatedNext = Coord {row = row next, col = col next, val = val next, ltr = True, rtl = rtl next, ttb = ttb next, btt = btt next}

step f (c, d) = mapMaybe (updatedNext f c) outDir
  where
    outDir = out (val c) d

process field [] = field
process field toEvaluate
  | null nextDirs = updatedNext
  | otherwise = process updatedNext nextDirs
  where
    nextDirs = concatMap (step field) toEvaluate
    updatedNext = updateCoordsInField field (map fst nextDirs)

markFirst f (c, d) = updateCoordInField f updatedC
  where
    updatedC = case d of
      RightD -> Coord {row = row c, col = col c, val = val c, ltr = True, rtl = rtl c, ttb = ttb c, btt = btt c}
      LeftD -> Coord {row = row c, col = col c, val = val c, ltr = ltr c, rtl = True, ttb = ttb c, btt = btt c}
      UpD -> Coord {row = row c, col = col c, val = val c, ltr = ltr c, rtl = rtl c, ttb = ttb c, btt = True}
      DownD -> Coord {row = row c, col = col c, val = val c, ltr = ltr c, rtl = rtl c, ttb = True, btt = btt c}

createCoord row = map (\(col, val) -> Coord {row = row, col = col, val = val, ltr = False, rtl = False, ttb = False, btt = False})

augmentCoordinates = zipWith createCoord [0 ..] . map (zip [0 ..])

solve x = maximum $ map (length . filter (== '#') . concat . fieldToStrings . uncurry process) initialInputs
  where
    field = augmentCoordinates $ lines x
    initialDirs =
      zip (head field) (repeat DownD)
        ++ zip (last field) (repeat UpD)
        ++ zip (map head field) (repeat RightD)
        ++ zip (map last field) (repeat LeftD)
    initialFields = map (markFirst field) initialDirs
    initialInputs = zip initialFields [[x] | x <- initialDirs]

main = readFile "input" >>= print . solve