import Data.List (sortBy, elemIndex, sort, group)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    bids <- map (makeBid . words) . lines <$> readFile "input_7.txt"
    solve Part1Hand bids
    solve Part2Hand bids

solve :: (Eq a, Hand a) => (Cards -> a) -> [Bid] -> IO ()
solve f = print . sum . zipWith getWinnings [1..] . (sortBy . comparing) (OrdHand . f . getCards)

data Bid = Bid Cards Amount deriving (Eq, Show)
newtype Part1Hand = Part1Hand Cards deriving (Eq, Show)
newtype Part2Hand = Part2Hand Cards deriving (Eq, Show)
type Amount = Int
type Cards  = String

makeBid :: [String] -> Bid
makeBid [h, v] = Bid h (read v)

getCards :: Bid -> Cards
getCards (Bid h _) = h

type Rank = Int
type Winnings = Int

getWinnings :: Rank -> Bid -> Winnings
getWinnings rank (Bid _ amount) = rank * amount

data Type = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving (Eq, Ord, Show)

class Hand a where
    toType :: a -> Type
    toCardValues :: a -> [Int]

sortedDeck1 :: Cards
sortedDeck1 = "23456789TJQKA"

wildcardReplacements :: Cards
wildcardReplacements = filter (/= 'J') sortedDeck1

sortedDeck2 :: Cards
sortedDeck2 = 'J':wildcardReplacements

newtype OrdHand a = OrdHand a deriving (Eq)

instance (Eq a, Hand a) => Ord (OrdHand a) where
    compare (OrdHand h) (OrdHand h') = comparing toType h h' <> comparing toCardValues h h'

instance Hand Part1Hand where
    toCardValues (Part1Hand h) = relativeCardValues sortedDeck1 h
    toType (Part1Hand h) = givenType h

instance Hand Part2Hand where
    toCardValues (Part2Hand h) = relativeCardValues sortedDeck2 h
    toType (Part2Hand h) = maximum . map (givenType . ($ h)) $ wildcardSubstitutors

relativeCardValues :: Cards -> Cards -> [Int]
relativeCardValues sortedDeck = fromMaybe [] . (traverse . (flip elemIndex)) sortedDeck

replace :: Char -> Char -> String -> String
replace x y = map (\a -> if (a == x) then y else a)

wildcardSubstitutors :: [Cards -> Cards]
wildcardSubstitutors = map (replace 'J') $ wildcardReplacements

givenType :: Cards -> Type
givenType = freqType . sortBy (flip compare) . map length . group . sort

freqType :: [Int] -> Type
freqType freq = case freq of
    [5]       -> FiveOfAKind
    [4,1]     -> FourOfAKind
    [3,2]     -> FullHouse
    [3,1,1]   -> ThreeOfAKind
    [2,2,1]   -> TwoPair
    [2,1,1,1] -> OnePair
    _         -> HighCard