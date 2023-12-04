import Data.List (tails, foldl')
import Data.Char (isDigit, digitToInt)

main :: IO ()
main = do
    contents <- readFile "input_3.txt"
    processCases (map (getSums . getContext)) id $ contents
    processCases (map getPowers) (map expandNumbers) $ contents

processCases :: ([(a, a, a)] -> [Int]) -> ([String] -> [a]) -> String -> IO ()
processCases f g = print . sum . f . matchUp . g . padLines . map pad . lines

pad :: String -> String
pad str = "." ++ str ++ "."

padLines :: [String] -> [String]
padLines strs = [padLine] ++ strs ++ [padLine]
    where
        padLine = replicate (length . head $ strs) '.'

matchUp :: [a] -> [(a, a, a)]
matchUp ls = zip3 ls (tail ls) (tail . tail $ ls)

getContext :: (String, String, String) -> [(Char, Bool)]
getContext (prev, curr, next) = zip (tail curr) summation
    where
        getRowContext = map (take 3) . tails
        rowContext = zipWith (++) (getRowContext prev) (getRowContext next)
        neighbours = zipWith (\a b -> [a, b]) curr (tail . tail $ curr)
        context = zipWith (++) rowContext neighbours
        isSymbol x = not (isDigit x) && (x /= '.')
        summation = map (any isSymbol) context ++ [False]

getSums :: [(Char, Bool)] -> Int
getSums = getThird . foldl' folder (False, 0, 0)
    where
        getThird (_, _, x) = x

folder :: (Bool, Int, Int) -> (Char, Bool) -> (Bool, Int, Int)
folder (soFar, curr, total) (c, t)
    | isDigit c = (soFar || t, 10 * curr + digitToInt c, total)
    | otherwise = (False, 0, total + if soFar then curr else 0)

expandNumbers :: String -> [String]
expandNumbers = concatMap (length >>= replicate) . snd . foldr readNumbers ("", [])

readNumbers :: Char -> (String, [String]) -> (String, [String])
readNumbers c (soFar, ls)
    | isDigit c = (c:soFar, ls)
    | otherwise = ("", [[c]] ++ (if soFar == "" then [] else [soFar]) ++ ls)

getPowers :: ([String], [String], [String]) -> Int
getPowers (prev, curr, next) = sum (zipWith (\a b -> if isGear a b then product b else 0) (tail curr) nums)
    where
        isNum = isDigit . head
        diag ls = zipWith3 (\a b c -> if isNum b then [b] else filter isNum [a, c]) 
                    ls (tail ls) (tail . tail $ ls)
        nei = zipWith (\a b -> filter isNum [a, b]) curr (tail . tail $ curr)
        context = zipWith3 (\a b c -> a ++ b ++ c) (diag prev) nei (diag next)
        nums = (map . map) (read :: String -> Int) context
        isGear a b = a == "*" && length b == 2
