import Data.List (transpose, notElem)

main :: IO ()
main = do
    universe <- lines <$> readFile "input_11.txt"

    let galaxies = map fst . filter ((=='#'). snd) . getCells $ universe
    let [emptyRows, emptyColumns] =  (getEmpty .) <$> [id, transpose] <*> pure universe
    let solve = pairDistancesSum emptyRows emptyColumns

    print . solve 2 $ galaxies
    print . solve 1000000 $ galaxies

getCells :: [String] -> [((Int, Int), Char)]
getCells = map rearrange . concat . zipWith ($) (map zip . map repeat $ [0..]) . (map . zip) [0..]
    where
        rearrange (a, (b, c)) = ((a, b), c)

pairDistancesSum :: [Int] -> [Int] -> Int -> [(Int, Int)] -> Int
pairDistancesSum emptyRows emptyColumns expansionFactor ls =
    (`div` 2) . sum $ [ abs (x - x') + abs (y - y') + xExpanse x x' + yExpanse y y' | (x, y) <- ls, (x', y') <- ls ]
    where
        expanse ls e1 e2 = (* (expansionFactor - 1)) . length . filter (\x -> x >= min e1 e2 && x <= max e1 e2) $ ls
        xExpanse = expanse emptyRows
        yExpanse = expanse emptyColumns

getEmpty :: [String] -> [Int]
getEmpty = map fst . filter (notElem '#' . snd) . zip [0..]
