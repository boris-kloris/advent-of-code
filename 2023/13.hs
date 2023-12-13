{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, splitOn, unpack)
import Data.List (transpose)
import Control.Arrow (first)

main :: IO ()
main = do
    patterns <- map (lines . unpack). splitOn "\n\n". pack <$> readFile "input_13.txt"
    print . sum . map (solve 0) $ patterns
    print . sum . map (solve 1) $ patterns

solve :: Int -> [String] -> Int
solve smudges pattern = countReflections smudges pattern * 100 + countReflections smudges (transpose pattern)

countReflections :: Int -> [String] -> Int
countReflections smudges ls =
      sum
    . map (length . fst)
    . filter (\(a, b) -> (== smudges) . sum $ zipWith diffNum a b)
    . map (first reverse)
    . map (($ ls) . splitAt)
    $ [1..(length ls - 1)]
    where
        diffNum x y = length . filter id $ zipWith (/=) x y
