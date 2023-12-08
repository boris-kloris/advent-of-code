import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM2)

main :: IO ()
main = do
    contents <- lines <$> readFile "input_8.txt"

    let instructions = concat . repeat . head $ contents
    let nodes = map readLink . tail . tail $ contents
    let driver = follow . fromList $ nodes
    print . step . getSteps driver (=="ZZZ") $ State "AAA" 0 instructions

    let endsWith c node = last node == c
    let startNodes = filter (endsWith 'A') . map fst $ nodes
    let startStates = zipWith3 State startNodes (repeat 0) (repeat instructions)
    let endStates = map (getSteps driver (endsWith 'Z')) startStates
    let periodicStates = map (period driver (endsWith 'Z')) endStates

    let unpack = liftM2 (,) (map node) (map step)
    let (endNodes, endSteps) = unpack endStates
    let (periodNodes, periodSteps) = unpack periodicStates
    let offsets = zipWith mod endSteps periodSteps

    -- THIS IS A HACK!!!
    putStrLn "Are all the offsets zero, and the 'Z' nodes are the only ones in the cycle?"
    putStrLn $ if all (==0) offsets && endNodes == periodNodes
        then "YES!!! The following answer is correct!"
        else "No, The following answer can't be trusted. :("
    print . foldr lcm 1 $ periodSteps

type Node = String
type Step = Int
type Instructions = String
type Turn = Char

data State = State { node :: Node, step :: Step, currentInstructions :: Instructions }

readLink :: String -> (Node, (Node, Node))
readLink str = (node 0 str, (node 7 str, node 12 str))
    where
        node pos = take 3 . drop pos

follow :: Map Node (Node, Node) -> Turn -> Node -> Node
follow map turn node = if turn == 'L' then left else right
    where
        (left, right) = fromMaybe ("", "") . Data.Map.lookup node $ map

getSteps :: (Turn -> Node -> Node) -> (Node -> Bool) -> State -> State
getSteps driver stopPredicate (State node num instructions@(turn:turns))
    | stopPredicate node = State node num instructions
    | otherwise = getSteps driver stopPredicate $ State (driver turn node) (num+1) turns

period :: (Turn -> Node -> Node) -> (Node -> Bool) -> State -> State
period driver stopPredicate (State node _ (turn:turns)) =
    getSteps driver stopPredicate (State (driver turn node) 1 turns)
