import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative(liftA2))
import Data.Map (Map, fromList, lookup)
import Data.List (find)

main :: IO ()
main = do
    contents <- lines <$> readFile "input_10.txt"

    let cells = getCells contents
    let grid = Data.Map.fromList cells
    let start = fst . head . filter ((=='S') . snd) $ cells

    let startNeighbours = map (move start) [Up .. Forth]
    let forwards = head . connected grid $ start

    let loop = fromMaybe [] . fmap snd . find fst . iterate (buildLoop grid start) $ (False, [forwards, start])

    print . (`div` 2) . (+1) . length $ loop

type Pipe         = Char
data Direction    = Up | Down | Back | Forth
    deriving (Eq, Show, Enum)
type Position     = (Int, Int)
type Displacement = (Int, Int)
type Grid         = Map Position Pipe

getCells :: [[Pipe]] -> [(Position, Pipe)]
getCells = map rearrange . concat . zipWith ($) (map zip . map repeat $ [0..]) . (map . zip) [0..]
    where
        rearrange (a, (b, c)) = ((a, b), c)

opposite :: Direction -> Direction
opposite d = case d of
    Up -> Down
    Down -> Up
    Back -> Forth
    Forth -> Back

connects :: Pipe -> [Direction]
connects p = case p of
    '-' -> [Back, Forth]
    '|' -> [Up, Down]
    '7' -> [Back, Down]
    'J' -> [Up, Back]
    'F' -> [Forth, Down]
    'L' -> [Up, Forth]
    _   -> []

displace :: Direction -> Displacement
displace d = case d of
    Up    -> (-1,  0)
    Down  -> ( 1,  0)
    Back  -> ( 0, -1)
    Forth -> ( 0,  1)

move :: Position -> Direction -> Position
move (x, y) dir = (x + dx, y + dy)
    where
        (dx, dy) = displace dir

getPipe :: Grid -> Position -> Pipe
getPipe grid = fromMaybe '.' . (flip Data.Map.lookup) grid

canConnect :: Grid -> Position -> [Direction]
canConnect grid position = filter (elem <*> map opposite . connects . getPipe grid . move position) $ [Up .. Forth]

connected :: Grid -> Position -> [Position]
connected = liftA2 map move . canConnect

connectsTo :: Grid -> Position -> [Position]
connectsTo grid currPos = map (move currPos) . connects . getPipe grid $ currPos

buildLoop :: Grid -> Position -> (Bool, [Position]) -> (Bool, [Position])
buildLoop _  _  state@(True, _) = state
buildLoop grid start (_, chain@(curr:prev:_)) =
    if next == start then (True, chain) else (False, next:chain)
    where
        [nei1, nei2] = connectsTo grid curr
        next = if nei1 == prev then nei2 else nei1
