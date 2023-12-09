main :: IO ()
main = do
    sequences <- (map . map) (read :: String -> Int) . map words . lines <$> readFile "input_9.txt"
    print . sum . map (sum . map last . extrapolate) $ sequences
    print . sum . map (foldr (-) 0 . map head . extrapolate) $ sequences

extrapolate :: [Int] -> [[Int]]
extrapolate = takeWhile (any (/= 0)) . iterate getDiffArray

getDiffArray :: [Int] -> [Int]
getDiffArray = zipWith (-) =<< tail