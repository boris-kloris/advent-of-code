{-# LANGUAGE OverloadedStrings #-}

import Data.Set (Set, fromList, intersection, size)
import Data.Text (pack, splitOn, drop, strip, unpack)
import Data.List (foldl')

main :: IO ()
main = do
    cards <- map makeCard . lines <$> readFile "input_4.txt"
    print . sum . map getPoints $ cards
    print . getNumCards . map getMatches $ cards

data Card = Card Int (Set Int) (Set Int)

makeCard :: String -> Card
makeCard line = Card id winners current
    where
        [header, desc] = splitOn ":" (pack line)
        id = read . unpack . strip . Data.Text.drop 5 $ header :: Int
        toSet = fromList . map (read :: String -> Int) . words . unpack . strip
        [winners, current] = map toSet . splitOn "|" $ desc
        
getMatches :: Card -> Int
getMatches (Card _ winners current) = size (intersection winners current)

getPoints :: Card -> Int
getPoints = (\matching -> if matching == 0 then 0 else 2 ^ (matching - 1)) . getMatches

getNumCards :: [Int] -> Int
getNumCards matches = snd . foldl' folder (cards, 0) $ matches
    where
        cards = replicate (length matches) 1

folder :: ([Int], Int) -> Int -> ([Int], Int)
folder ([],   total) _ = ([], total)
folder (n:ns, total) m = (zipWith (+) ns (replicate m n ++ repeat 0), total + n)
