{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, splitOn, unpack, strip, Text)
import Data.List (sortBy, foldl')
import Control.Applicative (liftA2)

main :: IO ()
main = do
    sections <- splitOn "\n\n" . pack <$> readFile "input_5.txt"

    let seeds = map (read :: String -> Int) . words . drop 6 . unpack . strip . head $ sections
    let maps  = map readMaps . tail $ sections
    print . minimum . map (($ maps) . foldl' mapOne) $ seeds

    let seedRanges = liftA2 zip (indices even) (indices odd) $ seeds
    print . minimum . map fst . (foldl' . flip) (concatMap . mapRange) seedRanges $ maps

type Map = [[Int]]
type Range = (Int, Int)

readMaps :: Text -> Map
readMaps = sortBy (\(_:a:_) (_:b:_) -> compare a b) . (map . map) (read :: String -> Int) . map words . tail . lines . unpack

mapOne :: Int -> Map -> Int
mapOne a [] = a
mapOne a ([dest, origin, range]:ms)
    | a < origin = a
    | a >= origin && a < origin + range = dest + (a - origin)
    | otherwise = mapOne a ms

indices :: (Int -> Bool) -> [a] -> [a]
indices f = map snd . filter (f . fst). zip [0..]

mapRange :: Map -> Range -> [Range]
mapRange [] (start, range) = if range <= 0 then [] else [(start, range)] 
mapRange m@([dest, origin, span]:ls) r@(start, range)
    | range <= 0       = []
    | gap <= (-range)  = [r]
    | gap < 0          = (start, (-gap)) : mapRange m (origin, range + gap)
    | gap >= span      = mapRange ls r
    | otherwise        = (dest + gap, min range (span - gap)) : mapRange ls (origin + span, range + gap - span)
    where
        gap = start - origin