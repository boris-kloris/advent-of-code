{-# LANGUAGE OverloadedStrings #-}

import System.IO (readFile)
import Data.Maybe (catMaybes)
import Data.Text as T (splitOn, pack, Text, drop, unpack, strip)

main :: IO ()
main = do
    contents <- readFile "input_2.txt"
    processCases (catMaybes . map playable) $ contents
    processCases (map power) $ contents

processCases :: (Show b, Foldable t, Num b) => ([Game] -> t b) -> String -> IO ()
processCases f = print . sum . f . map toGame . lines

data Game  = Game Int Draws
type Draws = [Draw]
type Draw  = (Int, Int, Int)

playable :: Game -> Maybe Int
playable (Game id draws) = if checkDraws draws then Just id else Nothing

power :: Game -> Int
power (Game _ draws) = r * g * b
    where
        (r, g, b) = minimalSet draws

checkDraws :: Draws -> Bool
checkDraws draws = r <= 12 && g <= 13 && b <= 14
    where
        (r, g, b) = minimalSet draws

minimalSet :: Draws -> Draw
minimalSet = foldr maxDraws (0, 0, 0)
    where
        maxDraws (r, g, b) (r1, g1, b1) = (max r r1, max g g1, max b b1)

toGame :: String -> Game
toGame line = Game id draws
    where
        [header, game] = splitOn ":" . pack $ line
        id = read . unpack . T.drop 5 $ header :: Int
        draws = readDraws game

readDraws :: Text -> Draws
readDraws = map readDraw . splitOn ";"

readDraw :: Text -> Draw
readDraw = foldr makeDraw (0, 0, 0) . map (splitOn " " . strip) . (splitOn ",")

makeDraw :: [Text] -> Draw -> Draw
makeDraw [numText, color] (r, g, b) =
    case color of
        "red"   -> (r + num, g, b)
        "green" -> (r, g + num, b)
        "blue"  -> (r, g, b + num)
    where
        num = read . unpack $ numText :: Int
makeDraw _ _ = error "Not supposed to happen"
