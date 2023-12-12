{-# LANGUAGE OverloadedStrings #-}
import Data.Text (splitOn, pack, unpack)
import Control.Applicative (Applicative(liftA2))
import Data.List (group, intercalate)

main :: IO ()
main = do
    puzzles <- map makePuzzle . map words . lines <$> readFile "input_12.txt"
    print . sum . map arrangements $ puzzles

    let newPuzzles = map makeNewPuzzle puzzles
    print ""

makePuzzle :: [String] -> (String, [Int])
makePuzzle [springs, nums] = (springs, map (read . unpack) . splitOn "," . pack $ nums)

arrangements :: (String, [Int]) -> Int
arrangements (row, groups) = length . filter ((==groups) . map length . filter ((=='#'). head) . group) . substitute $ row

substitute :: String -> [String]
substitute "" = [""]
substitute ('?':ls) = liftA2 (++) (map ('.':)) (map ('#':)) $ (substitute ls)
substitute (x:ls)   = map (x:) (substitute ls)

makeNewPuzzle :: (String, [Int]) -> (String, [Int])
makeNewPuzzle (row, groups) = (intercalate "?" . replicate 5 $ row, concat . replicate 5 $ groups)

