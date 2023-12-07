import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf, tails)
import Data.Foldable (asum)
import Data.Maybe (catMaybes)

getCalibration :: [Int] -> Int
getCalibration [] = 0
getCalibration ls = (head ls) * 10 + (last ls)

processCases :: Show b => ([String] -> b) -> String -> IO ()
processCases f = print . f . lines

calibrationSum ::  (String -> [Int]) -> [String] -> Int
calibrationSum f = sum . map (getCalibration . f)

getDigits1 :: String -> [Int]
getDigits1 = map digitToInt . filter isDigit

getDigits2 :: String -> [Int]
getDigits2 = catMaybes . map extractDigit . tails

digits :: [String]
digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

readWrittenDigit :: String -> Int -> String -> Maybe Int
readWrittenDigit representation digit str = 
    if isPrefixOf representation str
        then Just digit
        else Nothing

digitFuncs :: [String -> Maybe Int]
digitFuncs =  zipWith readWrittenDigit digits [0..]

extractDigit :: String -> Maybe Int
extractDigit "" = Nothing
extractDigit str@(h:_)
    | isDigit h = Just . digitToInt $ h
    | otherwise = asum . map ($ str) $ digitFuncs

main :: IO ()
main = do
    contents <- readFile "input_1.txt"
    processCases (calibrationSum getDigits1) $ contents
    processCases (calibrationSum getDigits2) $ contents