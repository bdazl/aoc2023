module Day3 where

import Grid
import Parser

import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP

-- turn strings into a raw map
type RawMap = Grid RawElem
data RawElem = Dot | N Int | Sym Char deriving Eq

-- coord map is used to figure out adjacent coordinates of symbols
type CoordMap = Grid CoordElem
type CoordElem = Maybe [Coord]

-- used to find values and count position/index
type NumCharCnt = (Maybe Int, Int)

type ValIdxs = (Int, [Int])
type ValCoords = (Int, [Coord])

elem' :: Char -> RawElem
elem' '.' = Dot
elem' c = if isDigit c then N (digitToInt c) else Sym c

parseLn :: String -> [RawElem]
parseLn = map elem'

parseRawMap :: [String] -> RawMap
parseRawMap = map parseLn

-- get all coordinates adjacent to symbols
symAdj :: RawMap -> [Coord]
symAdj xss = [c | cs <- coords, c <- cs] where
    -- remove Nothing and remove unJustify
    coords = [fromJust cs | css <- cgrid, cs <- css, isJust cs]
    -- cgrid will contain rim coordinate of symbols or Nothing
    cgrid = gridApply f xss
    dims = dimensions xss

    f :: RawMap -> Coord -> RawElem -> CoordElem
    f _ c (Sym _) = Just (filterInside dims (rimN c))
    f _ _ _       = Nothing


-- various parsers to get values and their lengths
digitLenP :: ReadP NumCharCnt
digitLenP = do
    ns <- munch1 isDigit
    let n = read ns
    let l = length ns
    return (Just n, l)

otherLenP :: ReadP NumCharCnt
otherLenP = do
    get -- we don't care about the value
    return (Nothing, 1)

-- either a parsed value (with a char count) or whatever char + 1
combiLenP :: ReadP NumCharCnt
combiLenP = choiceL [digitLenP, otherLenP]

-- given an unparsed row, parse its values and respective indices
parseValRow :: String -> [ValIdxs]
parseValRow xs = if isNothing ncs 
                 then []
                 else valRows 0 (fromJust ncs) where
    ncs = parse (many1 combiLenP) xs

    -- what an abomination...
    -- this returns a list of values and their indices
    valRows :: Int -> [NumCharCnt] -> [ValIdxs]
    valRows _ [] = []
    valRows n ((Just v, cnt):xs)  = (v, [n..(n+cnt-1)]) : valRows (n+cnt) xs
    valRows n ((Nothing, cnt):xs) = valRows (n+cnt) xs

rowIdxsToCoords :: Int -> [ValIdxs] -> [ValCoords]
rowIdxsToCoords y vs = [(v, [(x, y) | x <- xs]) | (v, xs) <- vs]

rowValCoords :: String -> Int -> [ValCoords]
rowValCoords xs y = rowIdxsToCoords y (parseValRow xs)

allValCoords :: [String] -> [ValCoords]
allValCoords rs = impl 0 rs where
    impl _ []     = []
    impl y (x:xs) = rowValCoords x y ++ impl (y+1) xs

anyLhsInRhs :: [Coord] -> [Coord] -> Bool
anyLhsInRhs [] _ = False
anyLhsInRhs (c:cs) rhs = if elem c rhs
                         then True
                         else anyLhsInRhs cs rhs

filterVals :: [ValCoords] -> [Coord] -> [Int]
filterVals vs cs = [v | (v, _) <- fvs] where
    -- filter ValCoords by if their respective coordinates
    -- exists within cs
    fvs = filter (\(v, xs) -> anyLhsInRhs xs cs) vs

solve :: [String] -> Int
solve xs = sum vals where
    vals = filterVals allvs symcs
    symcs = symAdj rawMap
    allvs = allValCoords xs
    rawMap = parseRawMap xs

solve' :: [String] -> Int
solve' _ = -1

main :: IO ()
main = do
    file <- readFile "input/day3.txt"
    let l = lines file
    print $ solve  l
    print $ solve' l

ex1 :: [String]
ex1 = ["467..114..",
       "...*......",
       "..35..633.",
       "......#...",
       "617*......",
       ".....+.58.",
       "..592.....",
       "......755.",
       "...$.*....",
       ".664.598.."]
