{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Basement.IntegralConv (charToInt)
import Data.List (findIndex)
import Data.List.Split (splitOn, splitOneOf)

hash = foldl (\current x -> ((current + charToInt x) * 17) `rem` 256) 0

lensMatches prefix s = sPrefix == prefix
  where
    sPrefix = head . splitOn "=" $ s

focalLength s = read $ splitOn "=" s !! 1

power index box = index * sum boxValues
  where
    augmented = zip [1 ..] $ map focalLength box
    boxValues = map (uncurry (*)) augmented

op boxes (x : xs)
  | null xs && opc == '-' = map (filter (not . lensMatches lhs)) boxes
  | null xs && opc == '=' = newBoxes
  | opc == '-' = op (map (filter (not . lensMatches lhs)) boxes) xs
  | opc == '=' = op newBoxes xs
  where
    lhs = head . splitOneOf "=-" $ x
    opc = head . filter (`elem` "=-") $ x
    rhs = last . splitOneOf "=-" $ x
    box = hash lhs
    relevantBox = boxes !! box
    indexInBox = findIndex (lensMatches lhs) relevantBox
    newBoxes = case indexInBox of
      Just idx -> take box boxes ++ [take idx relevantBox ++ [x] ++ drop (idx + 1) relevantBox] ++ drop (box + 1) boxes
      Nothing -> take box boxes ++ [relevantBox ++ [x]] ++ drop (box + 1) boxes

solve x = sum $ zipWith power [1 ..] resultingBoxes
  where
    resultingBoxes = op [[] | x <- [0 .. 256]] $ splitOn "," x

main = readFile "input" >>= print . solve