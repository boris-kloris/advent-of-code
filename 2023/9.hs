main :: IO ()
main = do
    sequences <- (map . map) (read :: String -> Int) . map words . lines <$> readFile "input_9.txt"
    print . sum . map (sum . map last . diffPyramid) $ sequences
    print . sum . map (foldr (-) 0 . map head . diffPyramid) $ sequences

diffPyramid :: [Int] -> [[Int]]
diffPyramid = (takeWhile . any) (/= 0) . iterate diffSequence

diffSequence :: [Int] -> [Int]
diffSequence = zipWith (-) =<< tail