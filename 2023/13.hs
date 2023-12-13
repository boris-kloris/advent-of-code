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
countReflections ls =
      sum
    . map (length . fst)
    . filter (\(a, b) -> and (zipWith (==) (reverse a) b))
    . map (($ ls) . splitAt)
    $ [1..(length ls - 1)]

countReflections2 :: [String] -> Int
countReflections2 ls =
      fromMaybe 0
    . find check
    . map (\((i, _), (j, _)) -> (i + j + 1) `div` 2)
    . filter (\((i, a), (j, b)) -> (i + j) `mod` 2 == 1 && diffNum a b == 1)
    . concatMap (map =<< (,) . head)
    . init
    . tails
    . zip [0..]
    $ ls
    where
        check pos = (\(a, b) -> diffNum (reverse a) b == 1) (splitAt pos ls)
        diffNum x y = length . filter id $ zipWith (/=) x y
