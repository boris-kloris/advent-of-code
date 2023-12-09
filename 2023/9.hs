main :: IO ()
main = do
    sequences <- map ( map (read :: String -> Int) . words ) . lines <$> readFile "input_9.txt"
    print . sum . map extrapolateForwards $ sequences
    print . sum . map extrapolateBackwards $ sequences

extrapolateForwards :: [Int] -> Int
extrapolateForwards = sum . map last . diffPyramid

extrapolateBackwards :: [Int] -> Int
extrapolateBackwards = foldr (-) 0 . map head . diffPyramid

diffPyramid :: [Int] -> [[Int]]
diffPyramid = (takeWhile . any) (/= 0) . iterate diffSequence

diffSequence :: [Int] -> [Int]
diffSequence = zipWith (-) =<< tail