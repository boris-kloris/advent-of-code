{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, splitOn, unpack, strip, Text)
import Data.List (sortBy, foldl', foldl1')
import Control.Monad (liftM2)

main :: IO ()
main = do
    sections <- splitOn "\n\n" . pack <$> readFile "input_5.txt"

    let seeds = map (read :: String -> Int) . words . drop 6 . unpack . strip . head $ sections
    let maps  = map readMaps . tail $ sections
    print . minimum . map (($ maps) . foldl' mapIt) $ seeds

    let seedRanges = liftM2 zip (indices even) (indices odd) $ seeds
    print .  minimum . map fst . foldl' (\s m -> concatMap (mapRange m) s) seedRanges $ maps

readMaps :: Text -> [[Int]]
readMaps = sortBy (\(_:a:_) (_:b:_) -> compare a b) . map (map (read :: String -> Int) . words) . tail . lines . unpack

mapIt :: Int -> [[Int]] -> Int
mapIt a [] = a
mapIt a ([dest, origin, range]:ms)
    | a < origin = a
    | a >= origin && a < origin + range = dest + (a - origin)
    | otherwise = mapIt a ms

indices :: (Int -> Bool) -> [a] -> [a]
indices f = map snd . filter (f . fst). zip [0..]

mapRange :: [[Int]] -> (Int, Int) -> [(Int, Int)]
mapRange [] (start, range) = if range <= 0 then [] else [(start, range)] 
mapRange m@([dest, origin, span]:ls) r@(start, range)
    | range <= 0              = []
    | start + range <= origin = [r]
    | start < origin          = (start, origin - start) : mapRange m (origin, range - (origin - start))
    | start >= origin + span  = mapRange ls r
    | otherwise               = (dest + (start - origin), min range (span - (start - origin))) : mapRange ls (origin + span, range - (origin + span - start))