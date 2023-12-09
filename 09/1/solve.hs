import Data.List.Split (splitOn)

differences [x] = []
differences (x : xs) = head xs - x : differences xs

buildSequence i [d] = i : [i + d]
buildSequence i (d : ds) = i : buildSequence (i + d) ds

allDifferences l
  | all (== 0) nextDiff = [nextDiff]
  | otherwise = nextDiff : allDifferences nextDiff
  where
    nextDiff = differences l

rebuild initials diffs
  | length initials == 1 = buildSequence (head initials) $ replicate (length (head diffs) + 1) 0
  | otherwise = buildSequence (head initials) $ rebuild (tail initials) (tail diffs)

processLine l = rebuild initials diffs
  where
    diffs = allDifferences l
    initials = head l : map head (take (length diffs - 1) diffs)

solve x = sum $ map (last . processLine . map read) x

main = readFile "input" >>= print . solve . map (splitOn " ") . lines