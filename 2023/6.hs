import Data.List (foldl1')

main :: IO ()
main = do
    contents <- lines <$> readFile "input_6.txt"
    let input = pure (getTail .) <*> [head, last] <*> pure contents

    print . product . (foldl1' . zipWith) numButtonTimes1 . map extract $ input
    print . foldl1' numButtonTimes2 . map glue $ input

getTail :: String -> String
getTail = tail . dropWhile (/= ':')

extract :: String -> [Int]
extract = map (read :: String -> Int) . words

glue :: String -> Int
glue = (read :: String -> Int) . filter (/= ' ')

numButtonTimes1 :: Int -> Int -> Int
numButtonTimes1 time maxDistance = length . filter (> maxDistance). map (distanceCovered time) $ [0..time]
    where
        distanceCovered maxTime buttonTime = buttonTime * (maxTime - buttonTime)

numButtonTimes2 :: Int -> Int -> Int
numButtonTimes2 t d = floor root1 - ceiling root2 + 1 - takeAwayIfInt root1 - takeAwayIfInt root2
    where
        discriminant = t * t - 4 * d
        rootD = sqrt . fromIntegral $ discriminant :: Double
        root1 = (fromIntegral t + rootD) / 2.0
        root2 = (fromIntegral t - rootD) / 2.0
        takeAwayIfInt x = if x == fromIntegral (round x) then 1 else 0
