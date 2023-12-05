import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

applyRule x rule
  | sourceStart <= x && x < (sourceStart + rangeLength) = x - sourceStart + destStart
  | otherwise = x
  where
    destStart = head rule
    sourceStart = rule !! 1
    rangeLength = rule !! 2

applyMapping mapping x
  | null changed = x
  | otherwise = head changed
  where
    changed = filter (/= x) . map (applyRule x) $ mapping

blockToMapping = map (map read . words) . tail . concat

solve x =
  minimum
    . map
      ( applyMapping humidityToLocation
          . applyMapping temperatureToHumidity
          . applyMapping lightToTemperature
          . applyMapping waterToLight
          . applyMapping fertilizerToWater
          . applyMapping soilToFertilizer
          . applyMapping seedToSoil
      )
    $ seeds
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