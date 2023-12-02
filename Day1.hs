module Day1 where

import Parser
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import Text.ParserCombinators.ReadP

rules :: [(String, Char)]
rules = [("one", '1'),
         ("two", '2'),
         ("three", '3'),
         ("four", '4'),
         ("five", '5'),
         ("six", '6'),
         ("seven", '7'),
         ("eight", '8'),
         ("nine", '9')]

-- Main parser of the above rules
-- Either one of the rules apply to the current substring, otherwise simply consume whatever Char we find
mainP :: ReadP Char
mainP = do
    let c = [punctureP i o | (i, o) <- rules]
    let c_dflt = c ++ [get]
    o <- choiceL c_dflt
    return o

-- Puncture the String i to the Char o
punctureP :: String -> Char -> ReadP Char
punctureP i o = do
    string i
    return o

-- Expand any substrings of "one", "two" etc to "1", "2", ... "9", and leave the rest of the Chars
expand :: String -> String
expand = fromJust . (parse (many1 mainP))

maybeint :: Char -> Maybe Int
maybeint x = if isDigit x
             then Just $ digitToInt x
             else Nothing

someints :: String -> [Int]
someints = (map fromJust) . (filter (/=Nothing)) . (map maybeint)

someints' :: String -> [Int]
someints' = someints . expand

combine :: [Int] -> Int
combine xs = (10 * head xs) + last xs

sol :: (String -> [Int] ) -> [String] -> Int
sol f = sum . map (combine . f)

solve :: [String] -> Int
solve = sol someints

solve' :: [String] -> Int
solve' = sol someints'

main :: IO ()
main = do
    file <- readFile "input/day1.txt"
    let l = lines file
    print $ solve l
    print $ solve' l

ex1 :: [String]
ex1 = ["1abc2", "pqr3stu8vwx","a1b2c3d4e5f", "treb7uchet"]

ex2 :: [String]
ex2 = ["two1nine",
       "eightwothree",
       "abcone2threexyz",
       "xtwone3four",
       "4nineeightseven2",
       "zoneight234",
       "7pqrstsixteen"]
