module Day1 where

import Data.Char
import Data.Maybe
import qualified Data.Text as T

rules :: [(String, String)]
rules = [("one", "1"),
         ("two", "2"),
         ("three", "3"),
         ("four", "4"),
         ("five", "5"),
         ("six", "6"),
         ("seven", "7"),
         ("eight", "8"),
         ("nine", "9")]

-- First attempt failed:
-- apply :: String -> String
-- apply x = foldr f x rules where
--    f (s,r) x = T.unpack $ T.replace (T.pack s) (T.pack r) (T.pack x)
--
-- This does not work, because some words are sneaky:
--      zoneight234 -> z 1 ight234
--      NOT zone8234

maybeint :: Char -> Maybe Int
maybeint x = if isDigit x
             then Just $ digitToInt x
             else Nothing

someints :: String -> [Int]
someints = (map fromJust) . (filter (/=Nothing)) . (map maybeint)

-- someints' :: String -> [Int]
-- someints' = someints . apply

combine :: [Int] -> Int
combine xs = (10 * head xs) + last xs

sol :: (String -> [Int] ) -> [String] -> Int
sol f = sum . map (combine . f)

solve :: [String] -> Int
solve = sol someints

solve' :: [String] -> Int
solve' _ = 1 -- sol someints'

main :: IO ()
main = do
    file <- readFile "input/day1.txt"
    let l = lines file
    print $ solve l
    print $ solve' l

example :: [String]
example = ["1abc2", "pqr3stu8vwx","a1b2c3d4e5f", "treb7uchet"]

ex2 :: [String]
ex2 = ["two1nine",
       "eightwothree",
       "abcone2threexyz",
       "xtwone3four",
       "4nineeightseven2",
       "zoneight234",
       "7pqrstsixteen"]

exres :: [Int]
exres = [12, 38, 15, 77]
