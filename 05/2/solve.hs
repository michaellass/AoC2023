import Data.List (isPrefixOf, nub, sort)
import Data.List.Split (splitOn)

applyRule pair rule
  | fst pair < sourceStart + rangeLength && snd pair > sourceStart = (fst pair + offset, snd pair + offset)
  | otherwise = pair
  where
    destStart = head rule
    sourceStart = rule !! 1
    rangeLength = rule !! 2
    offset = destStart - sourceStart

applyMapping mapping pair
  | null changed = pair
  | otherwise = head changed
  where
    changed = filter (/= pair) . map (applyRule pair) $ mapping

blockToMapping = map (map read . words) . tail . concat

sourceBoundaries rule = [sourceStart, sourceStart + rangeLength]
  where
    sourceStart = rule !! 1
    rangeLength = rule !! 2

makePairs list
  | null list = []
  | otherwise = (a, b) : makePairs (drop 2 list)
  where
    a = head list
    b = a + list !! 1

splitPair boundaries pair
  | null relevantBoundaries = [pair]
  | otherwise =
    (first, head relevantBoundaries) :
    splitPair (tail boundaries) (head relevantBoundaries, nextFirst)
  where
    first = fst pair
    nextFirst = snd pair
    relevantBoundaries = filter (> first) . filter (< nextFirst) $ boundaries

mappingToBoundaries = sort . nub . concatMap sourceBoundaries

solve x =
  minimum
    . map (fst . applyMapping humidityToLocation)
    . concatMap (splitPair (mappingToBoundaries humidityToLocation) . applyMapping temperatureToHumidity)
    . concatMap (splitPair (mappingToBoundaries temperatureToHumidity) . applyMapping lightToTemperature)
    . concatMap (splitPair (mappingToBoundaries lightToTemperature) . applyMapping waterToLight)
    . concatMap (splitPair (mappingToBoundaries waterToLight) . applyMapping fertilizerToWater)
    . concatMap (splitPair (mappingToBoundaries fertilizerToWater) . applyMapping soilToFertilizer)
    . concatMap (splitPair (mappingToBoundaries soilToFertilizer) . applyMapping seedToSoil)
    . concatMap (splitPair (mappingToBoundaries seedToSoil))
    $ makePairs seeds
  where
    seeds = map read . tail . splitOn " " . concat . concat . filter (\s -> "seeds:" `isPrefixOf` head s) $ x
    seedToSoil = blockToMapping . filter (\s -> "seed-to-soil map:" `isPrefixOf` head s) $ x
    soilToFertilizer = blockToMapping . filter (\s -> "soil-to-fertilizer map:" `isPrefixOf` head s) $ x
    fertilizerToWater = blockToMapping . filter (\s -> "fertilizer-to-water map:" `isPrefixOf` head s) $ x
    waterToLight = blockToMapping . filter (\s -> "water-to-light map:" `isPrefixOf` head s) $ x
    lightToTemperature = blockToMapping . filter (\s -> "light-to-temperature map:" `isPrefixOf` head s) $ x
    temperatureToHumidity = blockToMapping . filter (\s -> "temperature-to-humidity map:" `isPrefixOf` head s) $ x
    humidityToLocation = blockToMapping . filter (\s -> "humidity-to-location map:" `isPrefixOf` head s) $ x

main = readFile "input" >>= print . solve . splitOn [""] . lines