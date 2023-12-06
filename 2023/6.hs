main :: IO ()
main = do
    contents <- lines <$> readFile "input_6.txt"

    let times = tail . dropWhile (/= ':') . head $ contents
    let distances = tail . dropWhile (/= ':') . last $ contents

    print . product $ zipWith numDistances (extract times) (extract distances)
    print $ getNum (glue times) (glue distances)

extract :: String -> [Int]
extract = map (read :: String -> Int) . words

glue :: String -> Int
glue = (read :: String -> Int) . concat . words

distanceCovered :: Int -> Int -> Int
distanceCovered maxTime buttonTime = buttonTime * (maxTime - buttonTime)

numDistances :: Int -> Int -> Int
numDistances time maxDistance = length . filter (> maxDistance). map (distanceCovered time) $ [0..time]

getNum :: Int -> Int -> Int
getNum t d = floor root1 - ceiling root2 + 1 - takeAwayIfInt root1 - takeAwayIfInt root2
    where
        discriminant = t * t - 4 * d
        rootD = sqrt . fromIntegral $ discriminant :: Double
        root1 = (fromIntegral t + rootD) / 2.0
        root2 = (fromIntegral t - rootD) / 2.0
        takeAwayIfInt x = if x == fromIntegral (round x) then 1 else 0
