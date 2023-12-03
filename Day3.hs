module Day3 where

import Grid
import Parser

import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP

-- turn strings into a raw map
type RawMap = Grid RawElem
data RawElem = Dot | N Int | Sym Char deriving Eq

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

-- the horrible implementation of this is due to part 2 of this puzzle
symAdj :: RawMap -> [Coord]
symAdj xss = [c | (_, neighs) <- symAdj' Nothing xss, c <- neighs]

-- get all coordinates adjacent to symbols
-- returns a list of (symbol coordinate, [neighbour coordinate])
-- if mc is Nothing, all symbols count
-- else only the value of (fromJust mc) is considered
--
-- the horrible interface here is due to part 2 that messed it up...
symAdj' :: Maybe Char -> RawMap -> [(Coord, [Coord])]
symAdj' mc xss = coords where
    -- remove Nothing and remove unJustify
    coords = [fromJust cs | css <- cgrid, cs <- css, isJust cs]
    -- cgrid will contain rim coordinate of symbols or Nothing
    cgrid = gridApply f xss
    dims = dimensions xss

    allSyms _ = True
    singleSym c = c == (fromJust mc)
    symConsidered = if isNothing mc
                    then allSyms
                    else singleSym

    f :: RawMap -> Coord -> RawElem -> Maybe (Coord, [Coord])
    f _ c (Sym s) = if symConsidered s
                    then Just (c, filterInside dims (rimN c))
                    else Nothing
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
    allvs = allValCoords xs
    symcs = symAdj rawMap
    rawMap = parseRawMap xs

findPreRatio :: [ValCoords] -> (Coord, [Coord]) -> Maybe (Int, Int)
findPreRatio vs (_, cs) = if length fvs == 2
                          then Just (head fvs, last fvs)
                          else Nothing where
    fvs = filterVals vs cs

solve' :: [String] -> Int
solve' xs = sum mulRatios where
    mulRatios = map (\(x,y) -> x*y) ratios
    ratios  = map fromJust . filter (/=Nothing) $ maybRatios
    maybRatios = map (findPreRatio allvs) symadjs
    allvs = allValCoords xs
    symadjs = symAdj' (Just '*') rawMap
    rawMap = parseRawMap xs

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
