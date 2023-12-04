{-# LANGUAGE OverloadedStrings #-}

import Data.Set (Set, fromList, intersection, size)
import Data.Text (pack, splitOn, strip, unpack)
import Data.List (foldl')

main :: IO ()
main = do
    matches <- map (getMatches. makeCard) . lines <$> readFile "input_4.txt"
    print . sum . map getPoints $ matches
    print . getNumCards $ matches

data Card = Card (Set Int) (Set Int)

makeCard :: String -> Card
makeCard line = Card winners current
    where
        [_, desc] = splitOn ":" . pack $ line
        toSet = fromList . map (read :: String -> Int) . words . unpack . strip
        [winners, current] = map toSet . splitOn "|" $ desc
        
getMatches :: Card -> Int
getMatches (Card winners current) = size (intersection winners current)

getPoints :: Int -> Int
getPoints matches = if matches == 0 then 0 else 2 ^ (matches - 1)

getNumCards :: [Int] -> Int
getNumCards matches = snd . foldl' folder (cards, 0) $ matches
    where
        cards = replicate (length matches) 1

folder :: ([Int], Int) -> Int -> ([Int], Int)
folder ([],   total) _ = ([], total)
folder (n:ns, total) m = (zipWith (+) ns (replicate m n ++ repeat 0), total + n)
