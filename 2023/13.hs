{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, splitOn, unpack)
import Data.List (transpose, tails, find)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    patterns <- map (lines . unpack). splitOn "\n\n". pack <$> readFile "input_13.txt"
    print . sum . map (solve countReflections) $ patterns
    print . sum . map (solve countReflections2) $ patterns

solve :: ([String] -> Int) -> [String] -> Int
solve countReflections pattern = countReflections pattern * 100 + countReflections (transpose pattern)

countReflections :: [String] -> Int
countReflections ls = sum . map (length . fst). filter (\(a, b) -> and (zipWith (==) (reverse a) b)) . map (($ ls) . splitAt) $ [1..(length ls - 1)]

countReflections2 :: [String] -> Int
countReflections2 ls = fromMaybe 0 . find (check ls) . map (\(i, j) -> (i + j + 1) `div` 2) . map fst . filter (\((i,j), diff) -> (i + j) `mod` 2 == 1 && diff == 1) $ diffs
    where
        diffs = map (\((i, a), (j, b)) -> ((i, j), diffNum a b)) . concatMap (map =<< (,) . head) . init . tails $ zip [0..] ls
        check ls at = (\(a, b) -> diffNum (reverse a) b == 1) (splitAt at ls)
        diffNum x y = length . filter id $ zipWith (/=) x y
